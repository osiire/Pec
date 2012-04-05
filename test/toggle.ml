
module E = Pec.Event.Make (Pec.EventQueue.DefaultQueueM) (Pec.EventQueue.DefaultQueueI)
module S = Pec.Signal.Make (E)
open S.OP
open Pec.Event

let (!%) = Printf.sprintf
let (+>) f g = g f
let p s = Printf.printf "%s\n" s; flush stdout

let b1_click, send_b1_click = E.make ()
let b2_click, send_b2_click = E.make ()
let mode_click, send_mode_click = E.make ()
let clear_click, send_clear_click = E.make ()

(* 複合モード *)
let mode_on = S.fold (fun b () -> not b) false mode_click

(* b1 *)
let b1_on = S.return false
let when_b1_on e = E.filter (fun _ -> S.read b1_on) e
let when_b1_off e = E.filter (fun _ -> not (S.read b1_on)) e
let when_mode_on e = E.filter (fun _ -> S.read mode_on) e
let when_mode_off e = E.filter (fun _ -> not (S.read mode_on)) e
let go_on = when_b1_off b1_click
let go_off_1 = when_mode_on b1_click
let go_off_2 = when_mode_off (E.choose [b2_click])
let go_off_3 = when_b1_on clear_click
let b1_true = E.map (fun _ -> true) go_on
let b1_false = 
  E.choose [go_off_1; go_off_2; go_off_3]
  +> when_b1_on
  +> E.map (fun _ -> false)

(* 非破壊的代入 *)
let _ = 
  b1_on <=< S.fold (fun _ b -> b) false (E.choose [b1_true; b1_false])

let run_all () =
  while E.run () > 0 do () done

(* テスト開始 *)
let _ =
  run_all ();
  assert (S.read b1_on = false);
  S.event b1_on
  +> E.subscribe (fun b -> print_string (!%"%b\n" b));

  send_b1_click ();
  run_all ();
  assert (S.read b1_on = true);

  send_b1_click ();
  run_all ();
  assert (S.read b1_on = true);

  send_b2_click ();
  run_all ();
  assert (S.read b1_on = false);

  send_mode_click ();
  run_all ();
  assert (S.read b1_on = false);

  send_b1_click ();
  run_all ();
  assert (S.read b1_on = true);

  send_b1_click ();
  run_all ();
  assert (S.read b1_on = false);

  send_b1_click ();
  run_all ();
  assert (S.read b1_on = true);

  send_clear_click ();
  run_all ();
  assert (S.read b1_on = false)
