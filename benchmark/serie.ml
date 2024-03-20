let it = 100_000_000

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


let report title t c =
  print_string "===========================================================";
  print_newline () ;
  Printf.printf "%#d %s\n" it title;
  Printf.printf "Execution time: %fs\n" t;
  Printf.printf "memory consumption: %fwords\n" c;
  print_newline ()
  

let () =
  let t = Sys.time() in
  let job1 = Domain.spawn integer_serie_stress_test in
  let job2 = Domain.spawn numeric_serie_stress_test in
  let job3 = Domain.spawn string_serie_stress_test in
  let t3, c3 = Domain.join job3 in
  let t2, c2 = Domain.join job2 in
  let t1, c1 = Domain.join job1 in
  report "integer serie stress test" t1 c1;
  report "numeric serie stress test" t2 c2;
  report "string serie stress test" t3 c3;
  Printf.printf "Total execution time: %fs\n" (Sys.time() -. t)
