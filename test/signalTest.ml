
module E = Pec.Events.Make (Pec.EventQueue.Default)
module S = Pec.Signal.Make (E)

let run_all () =
  while E.run () > 0 do () done

let put_test () =
  let s = S.return 1 in
  assert( S.read s = 1 );
  S.put s 2;
  run_all ();
  assert( S.read s = 2 );
  S.put s 3;
  run_all ();
  assert( S.read s = 3 );
  S.put s 4;
  run_all ();
  assert( S.read s = 4 )

(*
let map_test () =
  let seq = ref [] in
  let cell = E.make () in
  let cell' = E.make () in
  let _ = E.switch cell' (E.map (fun x -> x + 1) (E.ievent cell)) in
  let _ = E.listen (fun v -> seq := v :: !seq) cell' in
  E.put cell 1;
  E.put cell 2;
  run_all ();
  assert ( List.rev !seq = [2;3])

let choose_test () =
  let seq = ref [] in
  let cell1 = E.make () in
  let cell2 = E.make () in
  let cell3 = E.make () in
  let _ = E.switch cell3 (E.choose [E.ievent cell1; E.ievent cell2]) in
  let _ = E.listen (fun v -> seq := v :: !seq) cell3 in
  E.put cell1 3;
  E.put cell2 4;
  run_all ();
  assert ( List.rev !seq = [3;4])
*)

(* let never_test () = *)
(*   E.subscribe (fun x -> assert false) E.never; *)
(*   run_all () *)

(* let join_test () = *)
(*   let seq = ref [] in *)
(*   let cell1, sender1 = E.make () in *)
(*   let cell2, sender2 = E.make () in *)
(*   let cell3, sender3 = E.make () in *)
(*   let e = E.join cell2 in *)
(*   E.subscribe (fun v -> seq := v :: !seq) e; *)
(*   sender2 cell1; *)
(*   sender1 9; *)
(*   sender1 3; *)
(*   sender2 cell3; *)
(*   sender3 10; *)
(*   sender3 11; *)
(*   run_all (); *)
(*   assert ( List.rev !seq = [9;3;10;11]) *)

let fold_test () =
  let e, sender = E.make () in
  let s = S.fold (+) 0 e in
  assert (S.read s = 0);
  sender 1;
  E.run_all ();
  assert (S.read s = 1);
  sender 2;
  E.run_all ();
  assert (S.read s = 3);
  sender 3;
  E.run_all ();
  assert (S.read s = 6)

(* let filter_test () = *)
(*   let seq = ref [] in *)
(*   let cell1, sender1 = E.make () in *)
(*   let e = E.filter (fun x -> x > 0 && x < 5) cell1 in *)
(*   E.subscribe (fun v -> seq := v :: !seq) e; *)
(*   sender1 ~-1; *)
(*   sender1 1; *)
(*   sender1 2; *)
(*   sender1 6; *)
(*   sender1 9; *)
(*   sender1 3; *)
(*   sender1 10; *)
(*   run_all (); *)
(*   assert ( List.rev !seq = [1;2;3]) *)

(* let map2_test () = *)
(*   let seq1 = ref [] in *)
(*   let seq2 = ref [] in *)
(*   let cell1, sender1 = E.make () in *)
(*   let e1 = E.map (fun x -> x + 1) cell1 in *)
(*   let e2 = E.map (fun x -> x + 2) cell1 in *)
(*   E.subscribe (fun v -> seq1 := v :: !seq1) e1; *)
(*   E.subscribe (fun v -> seq2 := v :: !seq2) e2; *)
(*   sender1 1; *)
(*   sender1 2; *)
(*   sender1 3; *)
(*   run_all (); *)
(*   assert ( List.rev !seq1 = [2;3;4]); *)
(*   assert ( List.rev !seq2 = [3;4;5])   *)

(* let jmap2_test () = *)
(*   let seq1 = ref [] in *)
(*   let seq2 = ref [] in *)
(*   let cell1, sender1 = E.make () in *)
(*   let cell2, sender2 = E.make () in *)
(*   let j = E.join cell1 in *)
(*   let e1 = E.map (fun x -> x + 1) j in *)
(*   let e2 = E.map (fun x -> x + 2) j in *)
(*   E.subscribe (fun v -> seq1 := v :: !seq1) e1; *)
(*   E.subscribe (fun v -> seq2 := v :: !seq2) e2; *)
(*   sender1 cell2; *)
(*   sender2 1; *)
(*   sender2 2; *)
(*   sender2 3; *)
(*   let cell3, sender3 = E.make () in *)
(*   sender1 cell3; *)
(*   sender2 4; *)
(*   sender3 5; *)
(*   sender3 6; *)
(*   run_all (); *)
(*   assert ( List.rev !seq1 = [2;3;4;6;7]); *)
(*   assert ( List.rev !seq2 = [3;4;5;7;8]) *)

(* let bind_test () = *)
(*   let seq = ref [] in *)
(*   let cell1, sender1 = E.make () in *)
(*   let cell2, sender2 = E.make () in *)
(*   let cell3, sender3 = E.make () in *)
(*   let e1 = E.map (fun x -> x + 1) cell1 in *)
(*   let e2 = E.map (fun x -> x + 2) cell2 in *)
(*   let e3 = E.bind cell3 (fun b -> if b then e1 else e2) in *)
(*   E.subscribe (fun v -> seq := v :: !seq) e3; *)
(*   sender1 1; *)
(*   sender2 2; *)
(*   sender3 true; *)
(*   sender1 3; *)
(*   sender2 4; *)
(*   sender1 5; *)
(*   sender2 6; *)
(*   sender3 false; *)
(*   sender1 7; *)
(*   sender2 8; *)
(*   sender1 9; *)
(*   sender2 10; *)
(*   run_all (); *)
(*   assert ( List.rev !seq = [4;6;10;12]) *)

(* let diamond_test () = *)
(*   let seq = ref [] in *)
(*   let cell1, sender1 = E.make () in *)
(*   let e1 = E.map (fun x -> x + 1) cell1 in *)
(*   let e2 = E.map (fun x -> x + 2) cell1 in *)
(*   let e3 = E.choose [e1; e2] in *)
(*   E.subscribe (fun v -> seq := v :: !seq) e3;   *)
(*   sender1 1; *)
(*   sender1 2; *)
(*   run_all (); *)
(*   let r = List.rev !seq in *)
(*   assert ( r = [2;3] || r = [2; 4] || r = [3; 3] || r = [3; 4]) *)

(* let zip_test () = *)
(*   let seq = ref [] in *)
(*   let cell1, sender1 = E.make () in *)
(*   let cell2, sender2 = E.make () in *)
(*   let e1 = E.zip cell1 cell2 in *)
(*   E.subscribe (fun v -> seq := v :: !seq) e1; *)
(*   run_all (); *)
(*   sender1 1; *)
(*   sender2 2; *)
(*   sender2 3; *)
(*   sender1 4; *)
(*   run_all (); *)
(*   assert ( List.rev !seq = [1,2; 1,3; 4,3] ) *)

(* let once_test () = *)
(*   let seq = ref [] in *)
(*   let cell1, sender1 = E.make () in *)
(*   E.subscribe (fun v -> seq := v :: !seq) (E.once cell1); *)
(*   sender1 1; *)
(*   sender1 2; *)
(*   sender1 3; *)
(*   sender1 4; *)
(*   run_all (); *)
(*   assert ( !seq = [1] ) *)

let tests =
  [
    "put test", put_test;
    (* "map test", map_test; *)
    (* "choose test", choose_test; *)
    (* "never test", never_test; *)
    (* "join test", join_test; *)
    "fold test", fold_test;
    (* "filter test", filter_test; *)
    (* "map2 test", map2_test; *)
    (* "jmap2 test", jmap2_test; *)
    (* "bind test", bind_test; *)
    (* "diamond test", diamond_test; *)
    (* "zip test", zip_test; *)
    (* "once test", once_test; *)
  ]

let (!%) = Printf.sprintf

let _ =
  Random.self_init ();
  List.iter (fun (name, f) ->
    print_string  (!%"running %s.." name);
    f ();
    print_string "done\n") tests
  
