module E = Pec.Event.Make (Pec.EventQueue.DefaultQueueM) (Pec.EventQueue.DefaultQueueI)
module S = Pec.Signal.Make (E)
module P = Wagon.WParser

open E.OP
let (+>) f g = g f
let (!%) = Printf.sprintf

let (b1 : unit E.t), b1_push = E.make ()
let (b2 : unit E.t), b2_push = E.make ()
let (b3 : unit E.t), b3_push = E.make ()
let (b4 : unit E.t), b4_push = E.make ()
let (b5 : unit E.t), b5_push = E.make ()
let (b6 : unit E.t), b6_push = E.make ()
let (b7 : unit E.t), b7_push = E.make ()
let (b8 : unit E.t), b8_push = E.make ()
let (b9 : unit E.t), b9_push = E.make ()
let (b0 : unit E.t), b0_push = E.make ()
let (plus : unit E.t), plus_push = E.make ()
let (minus : unit E.t), minus_push = E.make ()
let (enter : unit E.t), enter_push = E.make ()

let char_event = 
  let tn i e =
    E.map (fun _ -> i) e;
  in
  E.choose [
    tn '1' b1;
    tn '2' b2;
    tn '3' b3;
    tn '4' b4;
    tn '5' b5;
    tn '6' b6;
    tn '7' b7;
    tn '8' b8;
    tn '9' b9;
    tn '0' b0;
    tn '+' plus;
    tn '-' minus;
    tn 'e' enter;
  ]

let parser_signal parser init =
  char_event
  +> E.fold (fun (_, s) c -> 
    let s' = s ^ (!%"%c" c) in
    try
      Some (P.run_string parser s'), ""
    with
      _ -> None, s') (None, "")
  +> E.filter_map fst
  +> S.make init

let calculator_parser =
  let open P in
  let op =
    (char '+' +> map (fun _ -> `Plus)) <|> (char '-' +> map (fun _ -> `Minus))
  in
  int >>= (fun x -> 
    op >>= (fun op -> 
      (int <=< char 'e') >>= (fun y ->
        return (match op with `Plus -> x + y | `Minus -> x - y))))

let calculator = parser_signal calculator_parser 0

let _ =
  b1_push ();
  E.run_all ();
  assert (S.read calculator = 0);
  b0_push ();
  E.run_all ();
  minus_push ();
  E.run_all ();
  assert (S.read calculator = 0);
  b3_push ();
  enter_push ();
  E.run_all ();
  print_string (!%"10 - 3 = %d\n" (S.read calculator));
  b1_push ();
  b0_push ();
  plus_push ();
  b4_push ();
  enter_push ();
  E.run_all ();
  print_string (!%"10 + 4 = %d\n" (S.read calculator))
