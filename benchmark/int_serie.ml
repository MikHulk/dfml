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

let report title t c =
  print_string "===========================================================";
  print_newline () ;
  Printf.printf "%#d %s\n" it title;
  Printf.printf "Execution time: %fs\n" t;
  Printf.printf "memory consumption: %fwords\n" c;
  print_newline ()
  

let () =
  let t = Sys.time() in
  let t1, c1 = integer_serie_stress_test () in
  report "integer serie stress test" t1 c1;
  Printf.printf "Total execution time: %fs\n" (Sys.time() -. t)
