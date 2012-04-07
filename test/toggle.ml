
let (!%) = Printf.sprintf
let (+>) f g = g f
let p s = Printf.printf "%s\n" s; flush stdout

module E = Pec.Event.Make (Pec.EventQueue.DefaultQueueM) (Pec.EventQueue.DefaultQueueI)
module S = Pec.Signal.Make (E)
open S.OP
open Pec.Event

let b1_click, send_b1_click = E.make ()
let b2_click, send_b2_click = E.make ()
let mode_click, send_mode_click = E.make ()
let clear_click, send_clear_click = E.make ()

type on_off = ON | OFF
let flip = function ON -> OFF | OFF -> ON
let to_string = function ON -> "ON" | OFF -> "OFF"

(* 
 * b1 and b2 is ON-OFF button.
 * when mode is ON, b1 and b2 are independent toggle button.
 * when mode is OFF, b1 and b2 are exclusive button(like radio button).
 *)
let mode = S.fold (fun b () -> flip b) OFF mode_click
let when_on b e = E.filter (fun _ -> S.read b = ON) e
let when_off b e = E.filter (fun _ -> S.read b = OFF) e

let button_state self_clicked others_clicked =
  let state = S.return OFF in
  let to_on =
    when_off state self_clicked 
    +> E.map (fun _ -> ON)
  in
  let to_off = 
    E.choose [
      when_on mode self_clicked;
      when_off mode others_clicked;
      clear_click
    ]
    +> when_on state
    +> E.map (fun _ -> OFF)
  in
  state <=< S.fold (fun _ b -> b) OFF (E.choose [to_on; to_off]);
  state

let b1 = button_state b1_click (E.choose [b2_click])
let b2 = button_state b2_click (E.choose [b1_click])

(* テスト開始 *)
let _ =
  E.run_all ();
  assert (S.read b1 = OFF);
  S.event b1
  +> E.subscribe (fun b -> print_string (!%"%s\n" (to_string b)));

  send_b1_click ();
  E.run_all ();
  assert (S.read b1 = ON);

  send_b1_click ();
  E.run_all ();
  assert (S.read b1 = ON);

  send_b2_click ();
  E.run_all ();
  assert (S.read b1 = OFF);

  send_mode_click ();
  E.run_all ();
  assert (S.read b1 = OFF);

  send_b1_click ();
  E.run_all ();
  assert (S.read b1 = ON);

  send_b1_click ();
  E.run_all ();
  assert (S.read b1 = OFF);

  send_b1_click ();
  E.run_all ();
  assert (S.read b1 = ON);

  send_clear_click ();
  E.run_all ();
  assert (S.read b1 = OFF)


(* traditional way

let mode = ref OFF
let b1 = ref OFF
let b2 = ref OFF
let mode_getter () = !mode
let mode_setter b = mode := b
let b1_getter () = !b1
let b1_setter b = b1 := b
let b2_getter () = !b2
let b2_setter b = b2 := b

let on_b_clicked (getter, setter) others () =
  if getter () = ON then
    if mode_getter () = ON then
      setter OFF
    else
      ()
  else begin
    setter ON;
    if mode_getter () = ON then
      ()
    else
      List.iter (fun setter -> setter OFF) others
  end

let on_b1_clicked = on_b_clicked (b1_getter, b1_setter) [b2_setter]
let on_b2_clicked = on_b_clicked (b2_getter, b2_setter)  [b1_setter]

let on_clear_clicked () =
  b1_setter OFF;
  b2_setter OFF

let on_mode_clicked () =
  mode_setter (flip (mode_getter ()))
*)
