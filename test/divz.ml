
module E = Pec.Event.Make (Pec.EventQueue.DefaultQueueM) (Pec.EventQueue.DefaultQueueI)
open E.OP

let run_all () =
  while E.run () > 0 do () done

let (!%) = Printf.sprintf

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
  run_all ();
  (*print_string (String.concat "," (List.map string_of_int !seq));*)
  assert ( List.rev !seq = [100;50;25])

let tests =
  [ 
    "divz test", divz_test;
  ]

let (!%) = Printf.sprintf

let _ =
  Random.self_init ();
  List.iter (fun (name, f) ->
    print_string  (!%"running %s.." name);
    f ();
    print_string "done\n") tests;
  
