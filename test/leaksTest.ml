
module E = Pec.QueuedEvents.Make (Pec.QueuedEvents.Default)
module S = Pec.Signal.Make (E)

let (!%) = Printf.sprintf

let leak_test1 () =
  Gc.print_stat stdout;
  flush stdout;
  let e, sender_e = E.make () in
  let r = 
    S.fold (fun a f -> f a) 1 e
  in
  for i = 0 to 10000000 do
    sender_e (fun x -> x + 1);
    ignore (S.read r);
    Gc.full_major ();
    Gc.compact ();
  done;
  Thread.delay 0.5;
  Gc.print_stat stdout;
  flush stdout

let leak_test2 () =
  let e, sender_e = E.make () in
  let e2, sender_e2 = E.make () in
  let e3, sender_e3 = E.make () in
  let se = 
    E.switch e2
  in
  sender_e2 e;
  ignore (E.subscribe (fun x -> print_string (!%"%d\n" x);flush stdout) se);
  for i = 0 to 10000000 do
    sender_e (Random.int 100);
    if i = 10000 then begin
      sender_e2 e3;
      print_string "change to e3";
      flush stdout
    end;
    if i > 10000 then
      sender_e3 ((Random.int 100) + 100);
    Gc.full_major ();
    Gc.compact ();
  done;
  Thread.delay 0.5;
  Gc.print_stat stdout;
  flush stdout

let make_signal e sender =
  let s = S.fold (fun x () -> x + 1) 0 e in
  sender ();
  print_int (S.read s);
  flush stdout

let leak_test3 () =
  let e, sender_e = E.make () in
  for i = 0 to 10000000 do
    make_signal e sender_e;
    Gc.full_major ();
    Gc.compact ();
  done;
  Thread.delay 0.5;
  Gc.print_stat stdout;
  flush stdout

let tests =
  [ 
    (*"leak test1", leak_test1;
    "leak test2", leak_test2;*)
    "leak test3", leak_test3;
  ]

let _ =
  Random.self_init ();
  List.iter (fun (name, f) ->
    print_string  (!%"running %s.." name);
    f ();
    print_string "done\n") tests;
  
