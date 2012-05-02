module E = Pec.Event.Make (Pec.EventQueue.DefaultQueueM) (Pec.EventQueue.DefaultQueueI)
module S = Pec.Signal.Make (E)
open S.OP

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

type op = [ `Plus | `Minus ]
type state = [ `Init of int * int option | `Number of int * op * int | `OP of int * op ]

let tn i e =
  E.map (fun _ -> i) e

let number = 
  E.choose [
    tn 1 b1;
    tn 2 b2;
    tn 3 b3;
    tn 4 b4;
    tn 5 b5;
    tn 6 b6;
    tn 7 b7;
    tn 8 b8;
    tn 9 b9;
    tn 0 b0;
  ]

let op = 
  let tn i e =
    E.map (fun _ -> i) e;
  in
  E.choose [
    tn `Plus plus;
    tn `Minus minus;
  ]

let take_number = function
  | `Init (_, Some n) -> n
  | `Init (n, None) -> n
  | `Number (_, _, n) -> n
  | `OP (n , _) -> n

let take_number state =
  take_number (S.read state)

let append_n i n =
  match i, n with
    | 0, 0 -> 0
    | 0, n -> n
    | i, n -> int_of_string (!%"%d%d" i n)
      
let init_trans state =
  let when_init s e = 
    E.filter (fun _ -> match S.read s with `Init _ -> true | _ -> false) e
  in
  let append =
    when_init state number
    +> E.map (fun n -> 
      match S.read state with
        | `Init (_, Some i) -> `Init (0, Some (append_n i n))
        | `Init (_, None) -> `Init (0, Some n)
        | _ -> failwith "must not happen")
  in
  let op =
    when_init state op
    +> E.map (fun op -> `OP (take_number state, op))
  in
  E.choose [append; op]

let op_trans state =
  let when_op s e = 
    E.filter (fun _ -> match S.read s with `OP _ -> true | _ -> false) e
  in
  let op =
    when_op state op
    +> E.map (fun op -> `OP (take_number state, op))
  in
  let to_n =
    when_op state number
    +> E.map (fun n -> 
      match S.read state with
        | `OP (memory, op) ->
          `Number (memory, op, n)
        | _ -> failwith "must not happen")
  in
  E.choose [to_n; op]  

let calc x op y =
  match op with
    | `Plus -> x + y
    | `Minus -> x - y

let n_trans state =
  let when_n s e = 
    E.filter (fun _ -> match S.read s with `Number _ -> true | _ -> false) e 
  in
  let to_init =
    when_n state enter
    +> E.map (fun _ -> 
      match S.read state with
          `Number (memory, op , n) -> `Init ((calc memory op n), None)
        | _ -> failwith "must not happen")
  in
  let append =
    when_n state number
    +> E.map (fun n ->
      match S.read state with
          `Number (memory, op, i) -> `Number(memory, op, append_n i n)
        | _ -> failwith "must not happen")
  in
  let op =
    when_n state op
    +> E.map (fun op' ->
      match S.read state with
          `Number (memory, op, i) -> `OP (calc memory op i, op')
        | _ -> failwith "must not happen")
  in
  E.choose [to_init; append; op]
    

let state =
  let init =
    `Init (0, None)
  in
  let state = S.return init in  
  state <=< 
    S.fold (fun _ b -> b) init (E.choose [init_trans state; n_trans state; op_trans state]);
  state

let op2str = function
  | `Plus -> "+"
  | `Minus -> "-"

let display =
  S.map (function
    | `Init (i, None) -> !%"%d" i
    | `Init (_, Some i) -> !%"%d" i
    | `Number (_, op, i) -> !%"%s %d" (op2str op) i
    | `OP (i, op) -> !%"%s %d" (op2str op) i) state

let _ =
  b1_push ();
  E.run_all ();
  assert (S.read display = "1");
  b0_push ();
  E.run_all ();
  minus_push ();
  E.run_all ();
  assert (S.read display = "- 10");
  b3_push ();
  enter_push ();
  E.run_all ();
  print_string (!%"10 - 3 = %s\n" (S.read display));
  b1_push ();
  b0_push ();
  plus_push ();
  b4_push ();
  enter_push ();
  E.run_all ();
  print_string (!%"10 + 4 = %s\n" (S.read display))
