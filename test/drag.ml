
module E = Pec.Events
open E.OP

let (!%) = Printf.sprintf
let (+>) f g = g f

let mouse_down, send_down = E.make ()
let mouse_up, send_up = E.make ()
let mouse_move, send_move = E.make ()

let dragging md mu mm =
  md >>= (fun dloc -> E.choose [
    E.map (fun uloc -> `Drop (dloc, uloc)) mu; 
    E.map (fun mloc -> `Drag (dloc, mloc)) mm;
  ]
  +> E.take_while_in (function `Drop _ -> false | _ -> true))

let _ =
  dragging mouse_down mouse_up mouse_move
  +> E.subscribe (function 
    | `Drag (sloc, eloc) -> print_string (!%"Drag %d,%d\n" sloc eloc); flush stdout;
    | `Drop (sloc, eloc) -> print_string (!%"Drop %d,%d\n" sloc eloc); flush stdout;);
  while true do
    (match Random.int 3 with
      | 0 -> send_down (Random.int 10);
      | 1 -> send_move (Random.int 10);
      | 2 -> send_up (Random.int 10);
      | _ -> ());
    Thread.delay 0.01;
    Gc.full_major ();
    Gc.compact ()
  done;
  
