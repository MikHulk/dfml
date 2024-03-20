let it = 800_000_000

let time f =
    let t = Sys.time() in
    let (minor, major, promo) = Gc.counters () in
    let _ = f () in
    let (minor', major', promo') = Gc.counters () in
    let consumption = (minor' -. minor) +. (major' -. major) -. (promo' -. promo) in
    Sys.time() -. t, consumption
    
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

let wrapper title f =
  print_newline () ;
  print_string "===========================================================";
  print_newline () ;
  Printf.printf "%#d %s\n" it title;
  let (t, c) = f () in
  Printf.printf "Execution time: %fs\n" t;
  Printf.printf "memory consumption: %fwords\n" c;
  print_newline ()
  

let () =
  let t = Sys.time() in
  wrapper "integer serie stress test" integer_serie_stress_test ;
  wrapper "numeric serie stress test" numeric_serie_stress_test ;
  wrapper "string serie stress test" string_serie_stress_test ;
  Printf.printf "Total execution time: %fs\n" (Sys.time() -. t)
