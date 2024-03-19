let it = 80_000_000

let time f =
    let t = Sys.time() in
    let _ = f () in
    Printf.printf "Execution time: %fs\n" (Sys.time() -. t)
    
let integer_serie_stress_test () =
  let data = Array.init it (fun _ -> Random.int 100) in
  let f = fun () -> Array.to_seq data
      |> Dataframe.Serie.of_int_seq
      |> Dataframe.Serie.derive
        ( Dataframe.Ftype.from_int_to_int ( ( * ) 50) ) in
  time f

let numeric_serie_stress_test () =
  let data = Array.init it (fun _ -> Random.int 100) in
  let f = fun () -> Array.to_seq data
      |> Dataframe.Serie.nums_of_int_seq 2
      |> Dataframe.Serie.derive
        ( Dataframe.Ftype.from_float_to_float cos ) in
  time f

let string_serie_stress_test () =
  let data = Array.init it (fun _ -> Random.int 100) in
  let f = fun () -> Array.to_seq data
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
  print_string "integer serie stress test";
  print_newline () ;
  integer_serie_stress_test ();
  let (minor', major', promo') = Gc.counters () in
  let consumption = (minor' -. minor) +. (major' -. major) -. (promo' -. promo) in
  let (minor, major, promo) = (minor', major', promo') in
  Printf.printf "memory consumption: %fwords\n" consumption;
  print_newline () ;
  print_string "===========================================================";
  print_newline () ;
  
  print_string "numeric serie stress test";
  print_newline () ;
  numeric_serie_stress_test ();
  let (minor', major', promo') = Gc.counters () in
  let consumption =
    (minor' -. minor) +. (major' -. major) -. (promo' -. promo) in
  let (minor, major, promo) = (minor', major', promo') in
  Printf.printf "memory consumption: %fwords\n" consumption;
  print_newline () ;
  print_string "===========================================================";
  print_newline () ;
  
  print_string "string serie stress test";
  print_newline () ;
  string_serie_stress_test ();
  let (minor', major', promo') = Gc.counters () in
  let consumption = (minor' -. minor) +. (major' -. major) -. (promo' -. promo) in
  Printf.printf "memory consumption: %fwords\n" consumption;
  print_newline () ;
  print_string "===========================================================";
  print_newline () ;
  
  Printf.printf "Total execution time: %fs\n" (Sys.time() -. t)
