(*
   Down Down!
   $Id$

   to compile: 
     ocamlc -thread unix.cma threads.cma graphics.cma -I +pec pec.cma downdown.ml -w -5 -o downdown
*)
module U = Unix
module G = Graphics

let (+>) f g = g f
let (!%) = Printf.sprintf
let tee f x = ignore (f x); x
let debug s = print_string s;print_newline();flush Pervasives.stdout
let range _from _to _step =
  let rec loop s i =
    if i >= _to then List.rev s
    else loop (i :: s) (i + _step)
  in
  loop [] _from

let rec forever f x = 
  let v = f x in forever f v
    
let spawn_loop f x =
  ignore (Thread.create (fun () -> forever f x) ())

module SyncQueueI = struct
  type elem = unit -> unit
  type q = {
    queue : elem Queue.t;
    push_ch : elem Event.channel;
    take_ch : elem Event.channel;
  }
  let queue : q = {
    queue = Queue.create ();
    push_ch = Event.new_channel ();
    take_ch = Event.new_channel ();
  }
end

module SyncQueueM = struct
  type elem = SyncQueueI.elem
  type q = SyncQueueI.q

  let _ =
    let open Event in
    let q = SyncQueueI.queue in
    let open SyncQueueI in
    spawn_loop (fun () ->
      let ge =
        guard (fun () -> try send q.take_ch (Queue.peek q.queue) with _ -> choose [])
      in
      select [
        wrap (receive q.push_ch) (fun elem -> Queue.push elem q.queue);
        wrap ge (fun () -> ignore (Queue.take q.queue));
      ]) ()

  let push elem q =
    Event.sync (Event.send q.SyncQueueI.push_ch elem)
    
  let take q =
    Event.sync (Event.receive q.SyncQueueI.take_ch)

  let length q =
    Queue.length q.SyncQueueI.queue

end

module E = Pec.Event.Make (SyncQueueM) (SyncQueueI)
module S = Pec.Signal.Make (E)
open S.OP
    
let _map x e =
  E.map (fun _ -> x) e

let _filter x e =
  E.filter (fun y -> y = x) e

let _repeat x n =
  range 0 n 1
  +> List.map (fun _ -> x)

let _remove_last l =
  List.rev (List.tl (List.rev l))
    
let ystep = 20
let xmax = 32
let space = 5
let wait = 0.15
  
exception End
  
module Mode = struct
  type mode =
      Playing
    | GameOver

  type t = mode S.t

  let when_playing mode e =
    E.filter (fun _ -> (S.read mode) = Playing) e
      
  let when_gameover mode e = 
    E.filter (fun _ -> (S.read mode) = GameOver) e

end

module Score = struct
  type t = int S.t
      
  let make tick restart =
    E.choose [_map `Tick tick; _map `Restart restart]
    +> S.fold (fun s e -> 
      match e with
        | `Tick -> s + 1
        | `Restart -> 0) 0 
      
  let draw color score =
    G.set_color color;
    G.moveto 300 350;
    G.draw_string ("SCORE " ^ (string_of_int (S.read score)))
      
end

module Wall = struct

  type left = int
  type t = left list S.t

  let wall_y = 1
  let wall_height = 12
  let init_left =
    xmax / 2 - 1
  let wall_right left = 
    left + space + 1

  let make tick restart =
    let init =
      _repeat init_left wall_height
    in
    let next wall =
      let new_left wall =
        let w1, w2 =
          match wall with
	            h1 :: h2 :: _ -> h1, h2
            | _ -> assert false
        in
        let prob =
          if (wall_right w1) = xmax then
	          0
          else if w1 = 1 then
	          100
          else if (wall_right w1) > (xmax - xmax / 6) then
	          30
          else if w1 < (xmax / 6) then
	          70
          else if (w1 - w2) = 1 then
	          80
          else
	          20
        in
        if (Random.int 100) < prob then
          w1 + 1
        else
          w1 - 1
      in
      (new_left wall) :: (_remove_last wall)
    in
    E.choose [_map `Tick tick; _map `Restart restart]
    +> S.fold (fun s e -> 
      match e with
        | `Tick -> next s
        | `Restart -> init) init

  let draw color wall =
    let draw_wall_line color i left =
      let y =
        (wall_y + i) * ystep
      in
      G.set_color color;
      G.moveto 0 y;
      for i = 1 to left do
        G.draw_char '='
      done;
      for i = 1 to space do
        G.draw_char ' '
      done;
      for i = 1 to xmax - (wall_right left) + 1 do
        G.draw_char '='
      done
    in
    List.iteri (draw_wall_line color) (S.read wall)
      
end
  
module Arrow = struct
  
  let arrow_y = 5
  let arrow_height = 9
  let init_pos =
    xmax / 2 + space / 2

  type t = int list S.t

  let head t =
    List.hd (S.read t)

  let make key_event restart update mode =
    let initial =
      _repeat init_pos arrow_height
    in
    let move =
      Mode.when_playing mode key_event 
      +> E.filter_map (function
        | 'j' -> Some `Left
        | 'k' -> Some `Right
        | _ -> None)
    in
    E.choose [move; _map `Restart restart; _map `Tick update]
    +> S.fold (fun arrow dir ->
      match dir with
        | `Restart -> 
          initial
        | `Tick ->
          (List.hd arrow) :: (_remove_last arrow)
        | _ ->
          match arrow with
            | pos :: tl ->
              (if dir = `Left then pos - 1 else pos + 1) :: tl
            | _ ->
              arrow
    ) initial
            
  let draw color t =
    let draw_v color i pos =
      if i = 0 then
        G.set_color G.red
      else
        G.set_color color;
      let y =
        (arrow_y + i) * ystep
      in
      G.moveto 0 y;
      for i = 1 to pos - 1 do
        G.draw_char ' '
      done;
      G.draw_char 'V'
    in
    List.iteri (draw_v color) (S.read t)
      
end
    
type env = {
  mode : Mode.t;
  wall : Wall.t;
  arrow : Arrow.t;
  score : Score.t;
}

module Drawer = struct    
  let draw_title () =
    let open G in
    set_color black;
    moveto 30 350;
    draw_string "Down Down!";
    moveto 30 330;
    draw_string "left: j, right: k";
    moveto 30 310;
    draw_string "restart: r , quit: q"
      
  let draw env =
    G.clear_graph ();
    draw_title ();
    Wall.draw G.green env.wall;
    Arrow.draw G.green env.arrow;
    Score.draw G.green env.score;
    G.synchronize ()
      
end

let judge judge_tick restart env =
  let arrow_in_wall arrow wall =
    let pos =
      Arrow.head arrow
    in
    let left, right =
      let left =
        List.nth (S.read wall) (Arrow.arrow_y - 1)
      in
      left, left + space + 1
    in
    pos <= left || right <= pos
  in
  E.choose [_map `Tick judge_tick; _map `Restart restart]
  +> S.fold (fun s e -> 
    match e with
      | `Restart -> 
        Mode.Playing
      | `Tick ->
        if arrow_in_wall env.arrow env.wall then
          Mode.GameOver
        else
          Mode.Playing) Mode.Playing

let react tick_event key_event = 
  let mode =
    S.return Mode.Playing
  in
  let _ =
    _filter 'q' key_event
    +> E.subscribe (fun _ -> raise End)
  in
  let restart =
    Mode.when_gameover mode key_event
    +> _filter 'r'
  in
  let update_tick =
    Mode.when_playing mode tick_event
    +> _filter `Phase1
  in
  let judge_tick =
    _filter `Phase2 tick_event
  in
  let env = {
    mode;
    wall = Wall.make update_tick restart;
    arrow = Arrow.make key_event restart update_tick mode;
    score = Score.make update_tick restart;
  }
  in
  env.mode <=< judge judge_tick restart env;
  env

let make_key_event () =
  let key_event, key_send =
    E.make ()
  in
  spawn_loop (fun () ->
    let open G in
    let status =
	    wait_next_event [Key_pressed]
	  in
	  if status.keypressed then begin
	    key_send status.key
    end) ();
  key_event

let make_tick_event () =
  let tick_event, tick_send =
    E.make ()
  in
  spawn_loop (fun () ->
    Thread.delay wait;
    tick_send `Phase1;
    tick_send `Phase2;
    tick_send `Phase3) ();
  tick_event
    
let main () =
  Random.self_init ();
  G.open_graph " 640x400+50+50";
  G.set_font "-misc-fixed-bold-*-*-*-24-*-*-*-*-*-*-*";
  G.auto_synchronize false;
  let tick_event =
    make_tick_event ()
  in
  let env =
    react tick_event (make_key_event ())
  in
  _filter `Phase3 tick_event
  +> E.subscribe (fun _ -> Drawer.draw env)
  +> ignore;
  forever E.run_all ()

let _ = 
  try
    main()
  with
    End -> ()

