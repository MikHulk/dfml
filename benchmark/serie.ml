let it = 800_000_000

let time f =
    let t = Sys.time() in
    let _ = f () in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t)
    
let integer_serie_stress_test () =
  let f = fun () ->
    Seq.repeat 100
    |> Seq.take it
    |> Dataframe.Serie.of_int_seq
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_int_to_int ( ( * ) 50) ) in
  time f

let numeric_serie_stress_test () =
  let f = fun () ->
    Seq.repeat 100
    |> Seq.take it
    |> Dataframe.Serie.nums_of_int_seq 2
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_float_to_float cos ) in
  time f

let string_serie_stress_test () =
  let f = fun () ->
    Seq.repeat 100
    |> Seq.take it
    |> Seq.map Random.int
    |> Dataframe.Serie.nums_of_int_seq 2
    |> Dataframe.Serie.derive
      (Dataframe.Ftype.from_int_to_str Int.to_string) in
  time f

let () =
  let t = Sys.time() in
  let (minor, major, promo) = Gc.counters () in
  print_newline () ;
  
  print_string "===========================================================";
  print_newline () ;
  Printf.printf "%#d integers serie stress test\n" it;
  integer_serie_stress_test ();
  let (minor', major', promo') = Gc.counters () in
  let consumption = (minor' -. minor) +. (major' -. major) -. (promo' -. promo) in
  let (minor, major, promo) = (minor', major', promo') in
  Printf.printf "memory consumption: %#fwords\n" consumption;
  print_newline () ;
  print_string "===========================================================";
  print_newline () ;
  
  Printf.printf "%#d numerics serie stress test\n" it;
  print_newline () ;
  numeric_serie_stress_test ();
  let (minor', major', promo') = Gc.counters () in
  let consumption =
    (minor' -. minor) +. (major' -. major) -. (promo' -. promo) in
  let (minor, major, promo) = (minor', major', promo') in
  Printf.printf "memory consumption: %#fwords\n" consumption;
  print_newline () ;
  print_string "===========================================================";
  print_newline () ;
  
  Printf.printf "%#d strings serie stress test\n" it;
  print_newline () ;
  string_serie_stress_test ();
  let (minor', major', promo') = Gc.counters () in
  let consumption = (minor' -. minor) +. (major' -. major) -. (promo' -. promo) in
  Printf.printf "memory consumption: %#fwords\n" consumption;
  print_newline () ;
  print_string "===========================================================";
  print_newline () ;
  
  Printf.printf "Total execution time: %fs\n" (Sys.time() -. t)
