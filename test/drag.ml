
module E = Pec.Event.Make (Pec.EventQueue.DefaultQueueM) (Pec.EventQueue.DefaultQueueI)
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

let run_all () =
  while E.run () > 0 do () done

let _ =
  dragging mouse_down mouse_up mouse_move
  +> E.subscribe (function 
    | `Drag (sloc, eloc) -> print_string (!%"Drag %d,%d\n" sloc eloc)
    | `Drop (sloc, eloc) -> print_string (!%"Drop %d,%d\n" sloc eloc));
  send_down 10;
  send_move 11;
  send_move 12;
  send_up 13;
  send_move 14;
  send_move 15;
  send_down 16;
  send_move 17;
  send_move 18;
  send_up 19;
  run_all ()
