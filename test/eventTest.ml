
module E = Pec.Events
open E.OP

let (!%) = Printf.sprintf
let print s =
  print_string (!%"[%s]\n" (String.concat "," (List.map string_of_int (List.rev s))));
  flush stdout

let direct_test () =
  let seq = ref [] in
  let cell, sender = E.make () in
  let _ = E.subscribe (fun v -> seq := v :: !seq) cell in
  sender 1;
  sender 2;
  sender 3;
  sender 4;
  sender 5;
  (*print_string (String.concat "," (List.map string_of_int !seq));*)
  assert ( List.rev !seq = [1;2;3;4;5])

let map_test () =
  let seq = ref [] in
  let cell, sender = E.make () in
  let e = E.map (fun x -> x + 1) cell in
  let _ = E.subscribe (fun v -> seq := v :: !seq) e in
  sender 1;
  sender 2;
  assert ( List.rev !seq = [2;3;])

let choose_test () =
  let seq = ref [] in
  let cell1, sender1 = E.make () in
  let cell2, sender2 = E.make () in
  let e = E.choose [cell1; cell2] in
  let _ = E.subscribe (fun v -> seq := v :: !seq) e in
  sender1 3;
  sender2 4;
  assert ( List.rev !seq = [3;4])

let switch_test () =
  let seq = ref [] in
  let cell1, sender1 = E.make () in
  let cell2, sender2 = E.make () in
  let cell3, sender3 = E.make () in
  let e = E.switch cell2 in
  let _ = E.subscribe (fun v -> seq := v :: !seq) e in
  sender2 cell1;
  sender1 9;
  sender1 3;
  sender2 cell3;
  sender3 10;
  sender3 11;
  assert ( List.rev !seq = [9;3;10;11])

let filter_test () =
  let seq = ref [] in
  let cell1, sender1 = E.make () in
  let e = E.filter (fun x -> x > 0 && x < 5) cell1 in
  let _ = E.subscribe (fun v -> seq := v :: !seq) e in
  sender1 ~-1;
  sender1 1;
  sender1 2;
  sender1 6;
  sender1 9;
  sender1 3;
  sender1 10;
  assert ( List.rev !seq = [1;2;3])

let map2_test () =
  let seq1 = ref [] in
  let seq2 = ref [] in
  let cell1, sender1 = E.make () in
  let e1 = E.map (fun x -> x + 1) cell1 in
  let e2 = E.map (fun x -> x + 2) cell1 in
  let _ = E.subscribe (fun v -> seq1 := v :: !seq1) e1 in
  let _ = E.subscribe (fun v -> seq2 := v :: !seq2) e2 in
  sender1 1;
  sender1 2;
  sender1 3;
  assert ( List.rev !seq1 = [2;3;4]);
  assert ( List.rev !seq2 = [3;4;5])  

let smap2_test () =
  let seq1 = ref [] in
  let seq2 = ref [] in
  let cell1, sender1 = E.make () in
  let cell2, sender2 = E.make () in
  let j = E.switch cell1 in
  let e1 = E.map (fun x -> x + 1) j in
  let e2 = E.map (fun x -> x + 2) j in
  let _ = E.subscribe (fun v -> seq1 := v :: !seq1) e1 in
  let _ = E.subscribe (fun v -> seq2 := v :: !seq2) e2 in
  sender1 cell2;
  sender2 1;
  sender2 2;
  sender2 3;
  let cell3, sender3 = E.make () in
  sender1 cell3;
  sender2 4;
  sender3 5;
  sender3 6;
  (* print_string (String.concat "," (List.map string_of_int (List.rev !seq1))); *)
  (* print_newline(); *)
  (* print_string (String.concat "," (List.map string_of_int (List.rev !seq2))); *)
  (* print_newline(); *)
  assert ( List.rev !seq1 = [2;3;4;6;7]);
  assert ( List.rev !seq2 = [3;4;5;7;8])

let sbind_test () =
  let seq = ref [] in
  let cell1, sender1 = E.make () in
  let cell2, sender2 = E.make () in
  let cell3, sender3 = E.make () in
  let e1 = E.map (fun x -> x + 1) cell1 in
  let e2 = E.map (fun x -> x + 2) cell2 in
  let e3 = E.sbind cell3 (fun b -> if b then e1 else e2) in
  let _ = E.subscribe (fun v -> seq := v :: !seq) e3 in
  sender1 1;
  sender2 2;
  sender3 true;
  sender1 3;
  sender2 4;
  sender1 5;
  sender2 6;
  sender3 false;
  sender1 7;
  sender2 8;
  sender1 9;
  sender2 10;
  assert ( List.rev !seq = [4;6;10;12])

let diamond_test () =
  let seq = ref [] in
  let cell1, sender1 = E.make () in
  let e1 = E.map (fun x -> x + 1) cell1 in
  let e2 = E.map (fun x -> x + 2) cell1 in
  let e3 = E.choose [e1; e2] in
  let _ = E.subscribe (fun v -> seq := v :: !seq) e3 in
  sender1 1;
  sender1 2;
  let r = List.rev !seq in
  assert ( r = [2;3] || r = [2; 4] || r = [3; 3] || r = [3; 4])

let zip_test () =
  let seq = ref [] in
  let cell1, sender1 = E.make () in
  let cell2, sender2 = E.make () in
  let e1 = E.zip cell1 cell2 in
  let _ = E.subscribe (fun v -> seq := v :: !seq) e1 in
  sender1 1;
  sender2 2;
  sender2 3;
  sender1 4;
  sender1 5;
  sender1 6;
  (* print_string (!%"len=%d\n" (List.length !seq)); *)
  (* print_string (String.concat ";" (List.map (fun (v1,v2) -> !%"%d,%d" v1 v2) (List.rev !seq))); *)
  (* flush stdout; *)
  assert ( List.rev !seq = [1,2; 4,3;] )

let sequence_test () =
  let seq = ref [] in
  let cell1, sender1 = E.make () in
  let cell2, sender2 = E.make () in
  let e1 = E.sequence [cell1; cell2] in
  let _ = E.subscribe (fun v -> seq := v :: !seq) e1 in
  sender1 1;
  sender2 2;
  sender1 3;
  sender2 4;
  assert ( List.rev !seq = [[1;2]; [3;4];] )  

let once_test () =
  let seq = ref [] in
  let cell1, sender1 = E.make () in
  let _ = E.subscribe (fun v -> seq := v :: !seq) (E.once cell1) in
  sender1 1;
  sender1 2;
  sender1 3;
  sender1 4;
  assert ( !seq = [1] )

let immediate_test () =
  let seq = ref [] in
  let e, sender = E.make () in
  let e' =  e >>= (fun _ -> E.immediate 3) in
  let _ =  E.subscribe (fun v -> seq := v :: !seq) e' in
  sender 1;
  sender 2;
  assert ( !seq = [3;3] )

(* module QC = WQuickCheck *)

(* (\* for being able to test (int -> bool) *\) *)
(* module Testable_int_to_bool = *)
(*   QC.Testable_fun *)
(*     (\* for generating random int *\) *)
(*     (QC.Arbitrary_int) *)
(*     (\* for printing out int *\) *)
(*     (QC.PShow_int) *)
(*     (QC.Testable_bool) *)

(* module I2B = QC.Check(Testable_int_to_bool) *)

(* let monad_assoc_test () = *)
(*   let (>>=) = E.sbind in *)
(*   let m, sender = E.make () in *)
(*   let e1, sender_e1 = E.make () in *)
(*   let e2, sender_e2 = E.make () in *)
(*   let f x = E.map (fun v -> v - x) e1 in *)
(*   let g x = E.map (fun v -> v + x) e2 in *)
(*   let a = (m >>= f) >>= g in *)
(*   let b = m >>= (fun x -> (f x) >>= g) in *)
(*   let av = ref 0 in *)
(*   let bv = ref 0 in *)
(*   let _ = E.subscribe (fun v -> av := v) a in *)
(*   let _ = E.subscribe (fun v -> bv := v) b in *)
(*   let prop_a_b_same : int -> bool = *)
(*     fun i ->  *)
(*       let _ = *)
(*         match Random.int 3 with *)
(*           | 0 -> print_string (!%"sender %d\n" i); sender i *)
(*           | 1 -> print_string (!%"sender_e1 %d\n" i); sender_e1 i *)
(*           | 2 -> print_string (!%"sender_e2 %d\n" i); sender_e2 i *)
(*           | _ -> () *)
(*       in *)
(*       flush stdout; *)
(*       run_all (); *)
(*       !av = !bv *)
(*   in *)
(*   (\*I2B.verboseCheck { QC.quick with QC.maxTest = 100 } prop_a_b_same*\) *)
(*   I2B.check { QC.quick with QC.maxTest = 100 } prop_a_b_same *)

let divz_test () =
  let seq = ref [] in
  let e, sender = E.make () in
  let e' = E.map (fun x -> x = 0) e in
  let e'' = e' >>= (fun b -> if b then E.never else E.map (fun x -> 100 / x) e) in
  let _ = E.subscribe (fun v -> seq := v :: !seq) e'' in
  sender 1;
  sender 2;
  sender 0;
  sender 4;
  (*print_string (String.concat "," (List.map string_of_int !seq));*)
  assert ( List.rev !seq = [100;50;25])

let tests =
  [ 
    "direct test", direct_test;
    "map test", map_test;
    "choose test", choose_test;
    "switch test", switch_test;
    "filter test", filter_test;
    "map2 test", map2_test;
    "smap2 test", smap2_test;
    "sbind test", sbind_test;
    "diamond test", diamond_test;
    "zip test", zip_test;
    "once test", once_test;
    "immediate test", immediate_test;
    "sequence test", sequence_test;
    "divz test", divz_test;
  ]

let (!%) = Printf.sprintf

let _ =
  Random.self_init ();
  List.iter (fun (name, f) ->
    print_string  (!%"running %s.." name);
    f ();
    print_string "done\n") tests;
  
