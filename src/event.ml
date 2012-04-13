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
    
module Make ( M : EventQueue.M ) (I : EventQueue.I with type q = M.q ) = struct

  type cell_id = int
  type subscribe_id = int
  type time = int
      
  type 'a latest = {
    time : time;
    value : 'a option;
  }

  type 'a mcell = {
    id : cell_id;
    mutable data : 'a option;
    mutable notify : (subscribe_id * (cell_id -> time -> unit)) list;
  }
      
  and ('a, 'b) mwrap = {
    event : 'a event;
    wrap : 'a -> 'b;
    mutable w_latest : 'b latest option;
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

  and 'a event =
    | Cell : 'a mcell -> 'a event
    | Wrap : ('a, 'b) mwrap -> 'b event
    | Choose : 'a choose -> 'a event
    | Never : 'a event
    | Switch : 'a mswitch -> 'a event

  type 'a t = 'a event

  let get_id =
    let counter = ref 0 in
    fun () -> tee (fun _ -> incr counter) !counter

  let get_time =
    let counter = ref 0 in
    fun () -> tee (fun _ -> incr counter) !counter

  let run () =
    let _ =
      match maybe M.take I.queue with
        | `Val e -> e ()
        | _ -> ()
    in
    M.length I.queue

  let run_all () =
    while run () > 0 do () done

  let make () =
    let cell = {
      id = get_id ();
      data = None;
      notify = [];
    } 
    in
    Cell cell,
    (fun x ->
      M.push
        (fun () -> 
          let time = get_time () in
          cell.data <- Some x;
          let ns = cell.notify in
          cell.notify <- []; (* notify関数が再度cell.notifyをセットするはず。
                                これによりnotify関数を削除する手間を省く。*)
          List.iter (fun (_, notify) -> notify cell.id time) ns) I.queue)

  let map f e = Wrap {
    event = e;
    wrap = f;
    w_latest = None;
  }

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
    inner = [];
    s_latest = None
  }

  let set_notify_to_cell cell (id, notify) =
    match maybe (List.find (fun (id', _) -> id' = id)) cell.notify with
        `Val _ -> ()
      | `Err _ -> 
        debug (!%"set_notify! %d\n" cell.id);
        cell.notify <- (id, notify) :: cell.notify

  let rec set_notify : 'a. (subscribe_id * (cell_id -> time -> unit)) -> 'a event -> unit = 
    fun notify -> function
      | Cell cell ->
        set_notify_to_cell cell notify
      | Wrap w ->
        set_notify notify w.event
      | Choose c ->
        List.iter (set_notify notify) c.choose
      | Switch s ->
        set_notify notify s.outer;
        List.iter (set_notify notify) s.inner
      | _ -> ()

  let remove_cell_notify cell id =
    cell.notify <- List.filter (fun (id', _) -> id' <> id) cell.notify

  let rec remove_notify : 'a. subscribe_id -> 'a event -> unit = 
    fun id -> function
      | Cell cell ->
        remove_cell_notify cell id
      | Wrap w ->
        remove_notify id w.event
      | Choose c ->
        List.iter (remove_notify id) c.choose
      | Switch s ->
        remove_notify id s.outer;
        List.iter (remove_notify id) s.inner
      | _ -> ()

  let rec set_notify_with_id : 'a. cell_id -> (subscribe_id * (cell_id -> time -> unit)) -> 'a event -> bool = 
    fun id notify -> function
      | Cell cell ->
        if cell.id = id then begin
          set_notify_to_cell cell notify;
          true
        end else
          false
      | Wrap w ->
        set_notify_with_id id notify w.event
      | Choose c ->
        let rec set_notify_one = function
          | [] -> false
          | hd :: tl ->
            if set_notify_with_id id notify hd then
              true
            else
              set_notify_one tl
        in
        set_notify_one c.choose
      | Switch s ->
        if set_notify_with_id id notify s.outer then
          true
        else
          List.fold_left (fun flag inner -> set_notify_with_id id notify inner || flag) false s.inner
      | _ ->
        false

  let with_latest latest setter time reader set_notify =
    let get_sametime_cache latest time =
      match latest with
        | Some l when l.time = time -> l.value
        | _ -> None
    in
    match get_sametime_cache latest time with
        None ->
          (* 同じ時刻の呼び出しはキャッシュとして返せるようにしておく *)
          tee (fun v -> setter (Some { time = time; value = v })) (reader ())
      | v -> 
        (* ??? *)
        set_notify ();
        v

  let set_notify_only id notify e =
    fun () -> ignore (set_notify_with_id id notify e)


  let rec read : 'a. cell_id -> (subscribe_id * (cell_id -> time -> unit)) -> time -> 'a event -> 'a option = 
    fun id notify time -> function
      | Cell cell ->
        if cell.id = id then begin
          debug (!%"find id=%d\n" id);
          (* 次回も呼び出してもらえるようにnotifyを代入しておく *)
          set_notify_to_cell cell notify;
          cell.data
        end else begin
          debug (!%"id is deiff cell.id %d <> %d\n" cell.id id);
          None
        end
      | Wrap w ->
        debug "map\n";
        let wrap_read () =
          read id notify time w.event
          +> may_map (lazy None) (fun v -> Some (w.wrap v))
        in
        with_latest w.w_latest (fun l -> w.w_latest <- l) time 
          wrap_read 
          (set_notify_only id notify w.event)
      | Choose c as e ->
        let choose_read () =
          let rec find_value = function
          [] -> None
            | hd :: tl ->
              debug (!%"choose %d\n" id);
              read id notify time hd
              +> may_map (lazy (find_value tl)) (fun v -> Some v)
          in
          (* 最初に見つけたidにマッチするイベントの値を見つけてくる *)
          find_value c.choose
        in
        with_latest c.c_latest (fun l -> c.c_latest <- l) time 
          choose_read 
          (set_notify_only id notify e)
      | Switch s as e ->
        debug "switch";
        let rec read_one_of = function
          | [] -> None
          | [hd] -> 
            read id notify time hd
          | _ -> failwith "must not occur"
        in
        let switch_read () =
          match read id notify time s.outer with
            | None ->
              debug "try to find inner\n";
              read_one_of s.inner
            | Some inner ->
              debug "inner\n";
              s.inner <- [inner];
              if inner <> Never then begin
                ignore (set_notify notify inner);
                read id notify time inner
                +> tee (function 
                  | None -> debug "read switch inner returns none\n"
                  | Some _ ->  debug "switch same time inner is Some\n")
              end else
                None
        in
        with_latest s.s_latest (fun l -> s.s_latest <- l) time 
          switch_read 
          (set_notify_only id notify e)
      | Never -> None

  let subscribe f e =
    let subscribe_id = 
      get_id () 
    in
    let rec notify cell_id time =
      (*debug (!%"raise %d\n" id);*)
      (match read cell_id (subscribe_id, notify) time e with
          None -> ()
        | Some v -> f v);
      (*debug "one notify end\n";*)
    in
    ignore (set_notify (subscribe_id, notify) e);
    subscribe_id
    (*debug "subscribe end\n"*)

  let unsubscribe subscribe_id e =
    remove_notify subscribe_id e

  let sbind e f =
    switch (map f e)

  module OP = struct
    let (>>=) = sbind
  end

  open OP

  (* utitly functions *)

  let scan f i e =
    let s = ref i in
    map (fun x -> tee (fun n -> s := n) (f !s x)) e

  let return x =
    let e, sender = make () in
    sender x;
    e

  let _return x e =
    map (fun _ -> x) e

  let filter cond e =
    e >>= (fun x -> if cond x then e else never)

  let filter_map f e =
    e >>= (fun x -> match f x with Some v -> _return v e | None -> never)

  let zip e1 e2 =
    let extract q1 q2 =
      if WQ.is_empty q1 || WQ.is_empty q2 then
        q1, q2, None
      else
        WQ.tail q1, WQ.tail q2, Some (WQ.head q1, WQ.head q2)
    in
    choose [ map choice1 e1; map choice2 e2 ]
    +> scan (fun (q1, q2, _) v ->
      match v with
        | `T1 v -> 
          extract (WQ.push q1 v) q2
        | `T2 v -> 
          extract q1 (WQ.push q2 v)) (WQ.empty (), WQ.empty (), None)
    +> filter_map (fun (_, _, v) -> v)

  let sequence ms =
    let mcons ms m =
      m >>= (fun x -> ms >>= (fun y -> _return (x :: y) ms))
    in
    let r = List.rev ms in
    List.fold_left mcons (map (fun x -> [x]) (List.hd r)) (List.tl r)

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
        _return (Queue.pop q) e)

  let pairwise e =
    zip e (delay 1 e)

  let map2 f a b =
    zip a b +> map (fun (a, b) -> f a b)

  let map3 f a b c =
    zip a b +> zip c +> map (fun (c, (a, b)) -> f a b c)

  let map4 f a b c d =
    zip a b +> zip c +> zip d +> map (fun (d, (c, (a, b))) -> f a b c d)

  let map5 f a b c d e =
    zip a b +> zip c +> zip d +> zip e +> map (fun (e, (d, (c, (a, b)))) -> f a b c d e)

end
