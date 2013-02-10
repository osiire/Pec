(*
   Copyright (c) 2011 IT Planning inc. All Rights Reserved.

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

let (+>) f g = g f
let ($) f g x = f (g x)
let tee f x = f x; x
let maybe f x = try `Val (f x) with e -> `Err e
let value = function `Val v -> v | `Err e -> raise e
let may_map default f = function Some v -> f v | None -> Lazy.force default
let empty_map default f = function [] -> Lazy.force default | x -> f x
let id x = x
let (!%) = Printf.sprintf
let is_debug = ref false
let debug s = if !is_debug then print_string s else ()

type ('a, 'b) choice2 =
  [`T1 of 'a | `T2 of 'b]
type ('a, 'b, 'c) choice3 =
  [ ('a,'b) choice2 | `T3 of 'c ]
type ('a, 'b, 'c, 'd) choice4 =
  [ ('a,'b, 'c) choice3 | `T4 of 'd ]
type ('a, 'b, 'c, 'd, 'e) choice5 =
  [ ('a,'b, 'c, 'd) choice4 | `T5 of 'e ]

let choice1 x = `T1 x
let choice2 x = `T2 x
let choice3 x = `T3 x
let choice4 x = `T4 x
let choice5 x = `T5 x

module WQ = WQueue
let lmap f l = List.rev (List.rev_map f l)
let lmapi f l =
  let i = ref 0 in
  lmap (fun v -> tee (fun _ -> incr i) (f !i v)) l
let lappend a b =
  List.rev_append (List.rev a) b

module Option = struct
  let map f m = match m with None -> None | Some v -> Some (f v)
  let iter f m = match m with None -> () | Some v -> f v
end

type cell_id = int
type subscribe_id = int
type time = int

type 'a latest = {
  time : time;
  value : 'a option;
}

type notify = {
  subscribe_id : subscribe_id;
  follow : cell_id -> time -> unit;
  switch_level : int;
}

type 'a mcell = {
  id : cell_id;
  mutable data : 'a option;
  mutable notify : notify list;
}

and 'a mwrap = {
  mutable get : cell_id -> time -> 'a option;
  mutable w_latest : 'a latest option;
  set_notify : notify -> bool;
  remove_notify : subscribe_id -> unit;
}

and 'a choose = {
  choose : 'a event list;
  mutable c_latest : 'a latest option;
}

and 'a mswitch = {
  outer : 'a event event;
  mutable inner : 'a event list;
  mutable s_latest : 'a latest option;
}

and 'a mreturn = {
  mutable used : bool; (* returnは1サイクルしか使えない *)
  return : 'a;
}

and 'a event =
  | Cell of 'a mcell
  | Wrap of 'a mwrap
  | Choose of 'a choose
  | Never
  | Switch of 'a mswitch
  | Return of 'a mreturn

type 'a t = 'a event
type 'a channel = 'a mcell

(*
 * returnは1サイクルしか使えない.
 * 1サイクル中に使ったreturnにusedフラグを立てるクロージャーを格納するキュー.
 *)
let used_return = Queue.create ();

module Notify = struct
  let make id follow =
    {
      subscribe_id = id;
      follow = follow;
      switch_level = 0;
      (*
       * switch_levelは、このnotifyerがswitchを何段挟んだnotifyerかを示す数字
       * この段数が浅いものから順に呼び出される.
       *)
    }

  let incr_switch_level n =
    { n with switch_level = n.switch_level + 1 }

end

module Cell = struct
  let set_data cell x =
    cell.data <- Some x

  let append_notify cell notify =
    match maybe (List.find (fun n -> n.subscribe_id = notify.subscribe_id)) cell.notify with
      | `Val _ ->
          (* 既に同じnotifyが設定されているので重複登録しない.*)
          false
      | `Err _ ->
          debug (!%"set_notify! %d\n" cell.id);
          cell.notify <- lappend cell.notify [notify];
          true

  let remove_notify cell subscribe_id =
    cell.notify <- List.filter (fun n -> n.subscribe_id <> subscribe_id) cell.notify

  let call_notify cell time =
    let ns = cell.notify in
    cell.notify <- []; (* notify.follow関数が再度cell.notifyをセットするはず。
                          これによりnotify関数を削除する手間を省く。*)
      (* switch_levelが浅いものから順に呼び出す. これでswitchのアップデート順番を確保する. *)
    List.sort (fun n1 n2 -> compare n1.switch_level n2.switch_level) ns
    +> List.iter (fun notify -> notify.follow cell.id time)

end

let _id_generator () =
  let counter = ref 0 in
  fun () -> tee (fun _ -> incr counter) !counter

let get_id =
  _id_generator ()

let get_time =
  _id_generator ()

let new_channel () =
  {
    id = get_id ();
    data = None;
    notify = [];
  }

let push cell x =
  let time = get_time () in
  Cell.set_data cell x;
  Cell.call_notify cell time;
  Queue.iter (fun f -> f()) used_return;
  Queue.clear used_return

let events cell = Cell cell

let make () =
  let ch = new_channel () in
  events ch, push ch

let scramble l =
  let a = Array.of_list l in
  let len = Array.length a in
  if len = 0 then
    l
  else begin
    for i = len - 1 downto 1 do
      let j = Random.int (i + 1) in
      let temp = a.(i) in a.(i) <- a.(j); a.(j) <- temp
    done;
    Array.to_list a
  end

let choose es = Choose {
    (*choose = scramble es;*)
  choose = es;
  c_latest = None;
}

let never = Never

let switch ee = Switch {
  outer = ee;
  inner = []; (* 過去のinnerを全部記憶するとswitchはjoinになる.*)
  s_latest = None
}

let rec set_notify : 'a. notify -> 'a event -> bool =
  fun notify -> function
    | Cell cell ->
        Cell.append_notify cell notify
    | Wrap w ->
        w.set_notify notify
    | Choose c ->
        List.fold_left (fun b e -> set_notify notify e || b) false c.choose
    | Switch s ->
        let outer_b =
          set_notify notify s.outer
        in
        let notify' =
          Notify.incr_switch_level notify
        in
        List.fold_left (fun b e -> set_notify notify' e || b) outer_b s.inner
    | _ ->
        false

let rec remove_notify : 'a. subscribe_id -> 'a event -> unit =
  fun subscribe_id -> function
    | Cell cell ->
        Cell.remove_notify cell subscribe_id
    | Wrap w ->
        w.remove_notify subscribe_id
    | Choose c ->
        List.iter (remove_notify subscribe_id) c.choose
    | Switch s ->
        remove_notify subscribe_id s.outer;
        List.iter (remove_notify subscribe_id) s.inner
    | _ -> ()

let with_latest latest setter time reader =
  match latest with
    | Some l when l.time = time ->
        l.value
    | _ ->
        (* 同じ時刻の呼び出しはキャッシュとして返せるようにしておく *)
        tee (fun v -> setter (Some { time = time; value = v })) (reader ())

let rec read : 'a. cell_id -> time -> 'a event -> 'a option =
  fun id time -> function
    | Cell cell ->
        if cell.id = id then begin
          debug (!%"find id=%d\n" id);
          cell.data
        end else begin
          debug (!%"id is diff cell.id %d <> %d\n" cell.id id);
          None
        end
    | Wrap w ->
        w.get id time
    | Choose c ->
        let choose_read () =
          let rec find_value = function
            | [] -> None
            | hd :: tl -> begin
              debug (!%"choose %d\n" id);
              match read id time hd  with
                | None -> find_value tl
                | v -> v
            end
          in
          (* 最初に見つけたidにマッチするイベントの値を見つけてくる *)
          find_value c.choose
        in
        with_latest c.c_latest (fun l -> c.c_latest <- l) time choose_read
    | Switch s ->
        debug "switch\n";
        let switch_read () =
          debug "try to find inner\n";
          read id time s.outer
          +> Option.iter (fun inner ->
            debug "get inner\n";
            s.inner <- [inner]);  (* innerは一つに限定する. switchの定義. *)
          match s.inner with
            | [inner] -> (* s.innerリストへは単一要素のしか入っていないはず. *)
                read id time inner
            | _ ->
                None
        in
        with_latest s.s_latest (fun l -> s.s_latest <- l) time switch_read
    | Return x when not x.used ->
        Queue.add (fun () -> x.used <- true) used_return;
        Some x.return
    | Return _ -> None
    | Never -> None

let subscribe f e =
  let subscribe_id =
    get_id ()
  in
  let rec follow cell_id time =
    (*debug (!%"raise %d\n" id);*)
    read cell_id time e +> Option.iter f;
    ignore (set_notify (Notify.make subscribe_id follow) e);
    (*debug "one notify end\n";*)
  in
  ignore (set_notify (Notify.make subscribe_id follow) e);
  subscribe_id
  (*debug "subscribe end\n"*)

let unsubscribe subscribe_id e =
  remove_notify subscribe_id e

let async_read f e =
  let subscribe_id =
    get_id ()
  in
  let rec follow cell_id time =
    read cell_id time e +> Option.iter f;
    unsubscribe subscribe_id e
  in
  ignore (set_notify (Notify.make subscribe_id follow) e)

let map f e =
  let w = {
    get = (fun _ _ -> failwith "err");
    set_notify = (fun n -> set_notify n e);
    remove_notify = (fun id -> remove_notify id e);
    w_latest = None;
  } in
  w.get <- (fun id time ->
        debug "map\n";
        let wrap_read () =
          read id time e
          +> Option.map f
        in
        with_latest w.w_latest (fun l -> w.w_latest <- l) time wrap_read);
  Wrap w

let sbind e f =
  switch (map f e)

module OP = struct
  let (>>=) = sbind
end

open OP

(* utitly functions *)

let immediate x =
  Return { used = false; return = x }

let choice1 x = map choice1 x
let choice2 x = map choice2 x
let choice3 x = map choice3 x
let choice4 x = map choice4 x
let choice5 x = map choice5 x

let fold f i e =
  let s = ref i in
  map (fun x -> tee (fun n -> s := n) (f !s x)) e

let filter cond e =
  e >>= (fun x -> if cond x then e else never)

let filter_map f e =
  e >>= (fun x -> match f x with Some v -> immediate v | None -> never)

let sequence ms =
  let qs = lmap (fun _ -> WQ.empty ()) ms in
  let extract qs i v =
    let qs =
      lmapi (fun j q -> if j = i then WQ.push q v else q) qs
    in
    if List.for_all (fun q -> not (WQ.is_empty q)) qs then
      lmap WQ.tail qs, Some (lmap WQ.head qs)
    else
      qs, None
  in
  lmapi (fun i e -> map (fun v -> (i, v)) e) ms
  +> choose
  +> fold (fun (qs, _) (i, v) -> extract qs i v) (qs, None)
  +> filter_map snd

let zip e1 e2 =
  sequence [choice1 e1; choice2 e2]
  +> map (function [`T1 v1; `T2 v2] -> v1, v2 | _ -> failwith "must not happen")

let take_while cond e =
  let flag = ref true in
  e >>= (fun v ->
    if !flag then
      if tee (fun b -> flag := b) (cond v) then
        e
      else
        never
    else
      never)

let take_while_in cond e =
  let flag = ref true in
  e >>= (fun v ->
    if !flag then begin
      flag := (cond v);
      e
    end else
      never)

let take_n n e =
  let cnt = ref 0 in
  take_while (fun _ -> tee (fun _ -> incr cnt) (!cnt < n)) e

let once e =
  take_n 1 e

let drop_while cond e =
  let flag = ref true in
  e >>= (fun v ->
    if !flag then
      if tee (fun b -> flag := b) (cond v) then
        never
      else
        e
    else
      e)

let drop_n n e =
  let cnt = ref 0 in
  drop_while (fun _ -> tee (fun _ -> incr cnt) (!cnt < n)) e

let delay n e =
  let q = Queue.create () in
  e >>= (fun v ->
    Queue.push v q;
    if Queue.length q < n then
      never
    else
      immediate (Queue.pop q))

let pairwise e =
  zip e (delay 1 e)

let split e f =
  filter f e, filter (fun x -> not (f x)) e

let split_n e n =
  let current_index = ref 0 in
  let before_e =
    map (tee (fun _ -> current_index := Random.int n)) e
  in
  WSeq.unfold (fun i ->
    if i < n then
      Some (filter (fun _ -> i = !current_index) before_e, i + 1)
    else
      None) 0
  +> WSeq.to_list

(* to convert event to lazy list. *)

module type LazyListSig = sig
  type 'a t
  val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b t
end

module Import (L : LazyListSig) = struct
  module Q = Queues.SyncQueue.Poly

  let to_llist e =
    let q = Q.create () in
    let _ =
      subscribe (fun v -> Q.push v q)
    in
    L.unfold (fun () -> Some (Q.take q, ())) ()
end


let map2 f a b =
  sequence [choice1 a; choice2 b]
  +> map (function [`T1 a; `T2 b] -> f a b | _ -> failwith "must not happen")

let map3 f a b c =
  sequence [choice1 a; choice2 b; choice3 c]
  +> map (function [`T1 a; `T2 b; `T3 c] -> f a b c | _ -> failwith "must not happen")

let map4 f a b c d =
  sequence [choice1 a; choice2 b; choice3 c; choice4 d]
  +> map (function [`T1 a; `T2 b; `T3 c; `T4 d] -> f a b c d | _ -> failwith "must not happen")

let map5 f a b c d e =
  sequence [choice1 a; choice2 b; choice3 c; choice4 d; choice5 e]
  +> map (function [`T1 a; `T2 b; `T3 c; `T4 d; `T5 e] -> f a b c d e | _ -> failwith "must not happen")

(*
let spawn f x =
  ignore (Thread.create f x)

let rec forever f x =
  let v = f x in forever f v

let spawn_loop f x =
  ignore (Thread.create (fun () -> forever f x) ())

let future g x =
  let e, sender = make () in
  spawn (fun () ->
    sender (g x)
  ) ();
  e

let timeout limit x =
  let e, sender = make () in
  spawn (fun () ->
    Thread.delay limit;
    sender x
  ) ();
  e

let repeat_timeout limit x =
  let e, sender = make () in
  spawn_loop (fun () ->
    Thread.delay limit;
    sender x
  ) ();
  e
*)
