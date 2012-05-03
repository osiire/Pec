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

(* module Wall = struct *)
  
(*   let wall_y = 1 *)
(*   let wall_height = 12 *)
    
(*   type wall_left = int *)
(*   type t = wall_left list R.t * (wall_left -> unit) *)
      
(*   let init_left = *)
(*     xmax / 2 - 1 *)
      
(*   let init (_, sender) = *)
(*     for i = 0 to wall_height - 1 do *)
(*       sender init_left *)
(*     done *)
      
(*   let wall = *)
(*     let r, sender =  *)
(*       R.make init_left *)
(*     in *)
(*     tee init (R.history wall_height r, sender) *)
      
(*   let wall_right left = *)
(*     left + space + 1 *)
      
(*   let draw_wall_line color i left = *)
(*     let y = *)
(*       (wall_y + i) * ystep *)
(*     in *)
(*     set_color color; *)
(*     moveto 0 y; *)
(*     for i = 1 to left do *)
(*       draw_char '=' *)
(*     done; *)
(*     for i = 1 to space do *)
(*       draw_char ' ' *)
(*     done; *)
(*     for i = 1 to xmax - (wall_right left) + 1 do *)
(*       draw_char '=' *)
(*     done *)
      
(*   let draw color (wall, _) = *)
(*     iteri (draw_wall_line color) (R.read wall) *)
      
(*   let space pos (wall, _) = *)
(*     let left = *)
(*       List.nth (R.read wall) pos *)
(*     in *)
(*     left, left + space + 1 *)
      
(*   let new_left wall = *)
(*     let w1, w2 = *)
(*       match R.read wall with *)
(* 	    h1 :: h2 :: _ -> h1, h2 *)
(*       | _ -> assert false *)
(*     in *)
(*     let prob = *)
(*       if (wall_right w1) = xmax then *)
(* 	    0 *)
(*       else if w1 = 1 then *)
(* 	    100 *)
(*       else if (wall_right w1) > (xmax - xmax / 6) then *)
(* 	    30 *)
(*       else if w1 < (xmax / 6) then *)
(* 	    70 *)
(*       else if (w1 - w2) = 1 then *)
(* 	    80 *)
(*       else *)
(* 	    20 *)
(*     in *)
(*     if (Random.int 100) < prob then *)
(*       w1 + 1 *)
(*     else *)
(*       w1 - 1 *)
        
(*   let update (wall, sender) = *)
(*     sender (new_left wall); *)
(*     wall, sender *)
      
(* end *)
  
module Arrow = struct
  
  let arrow_y = 5
  let arrow_height = 9
  let init_pos =
    xmax / 2 + space / 2

  type t = int list S.t

  let _remove_last l =
    List.rev (List.tl (List.rev l))

  let _make_list x n =
    let rec loop s n =
      if n = 0 then s
      else loop (x :: s) (n - 1)
    in
    loop [] n

  let make key_event restart mode =
    let initial =
      _make_list init_pos arrow_height
    in
    let move =
      Mode.when_playing mode key_event 
      +> E.filter_map (function
        | 'j' -> Some `Left
        | 'k' -> Some `Right
        | _ -> None)
    in
    E.choose [move; _map `Restart restart]
    +> S.fold (fun arrow dir ->
      match dir with
        | `Restart -> initial
        | _ ->
          let pos, tl = 
            List.hd arrow, _remove_last (List.tl arrow)
          in
          (if dir = `Left then pos - 1 else pos + 1) :: pos :: tl) initial
      
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
      
  let draw color t =
    List.iteri (draw_v color) (S.read t)
      
end
    
type env = {
  mode : Mode.t;
  (* wall : Wall.t; *)
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
    (* Wall.draw green env.wall; *)
    Arrow.draw G.green env.arrow;
    Score.draw G.green env.score;
    G.synchronize ()
      
  (* let update env = *)
  (*   match Mode.get () with *)
  (*       Mode.Playing -> *)
  (*         Gc.full_major(); *)
  (*         Gc.compact(); *)
  (*         Thread.delay 0.15; *)
  (*         let left, right = *)
	(*           Wall.space Arrow.pos_y env.wall *)
  (*         in *)
  (*         let pos =  *)
	(*           Arrow.move_down env.arrow; *)
  (*         in *)
  (*         if pos <= left || right <= pos then *)
	(*           tee (fun _ -> draw env; Mode.game_over()) env *)
  (*         else begin *)
	(*           Score.count_up env.score; *)
	(*           draw env; *)
	(*           { env with wall = Wall.update env.wall } *)
  (*         end *)
  (*     | Mode.GameOver -> *)
  (*       tee (fun _ -> draw env; Mode.wait_play ();Thread.delay 0.15;) env *)
end

let judge judege_tick restart env =
  S.return Mode.Playing

let react tick_event key_event = 
  let mode =
    S.return Mode.Playing
  in
  let _ =
    E.filter (fun c -> c = 'q') key_event
    +> E.subscribe (fun _ -> raise End)
  in
  let restart =
    Mode.when_gameover mode key_event
    +> E.filter (fun c -> c = 'r')
  in
  let update_tick, judge_tick =
    E.filter (fun t -> t = `Phase1) tick_event,
    E.filter (fun t -> t = `Phase2) tick_event
  in
  let env = {
    mode;
    (* env.wall <=< Wall.make update_tick restart; *)
    arrow = Arrow.make key_event restart mode;
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
  E.filter (function `Phase3 -> true | _ -> false) tick_event
  +> E.subscribe (fun _ -> Drawer.draw env)
  +> ignore;
  forever E.run_all ()

let _ = 
  try
    main()
  with
    End -> ()

