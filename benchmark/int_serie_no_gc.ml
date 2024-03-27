let it = 100_000_000

let time f =
    let t = Sys.time() in
    let _ = f () in
    Sys.time() -. t
    
let integer_serie_stress_test () =
  let f = fun () ->
    Seq.repeat 100
    |> Seq.take it
    |> Dataframe.Serie.of_int_seq
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_int_to_int ( ( * ) 50) ) in
  time f

let report title t =
  print_string "===========================================================";
  print_newline () ;
  Printf.printf "%#d %s\n" it title;
  Printf.printf "Execution time: %fs\n" t;
  print_newline ()
  

let () =
  let t = Sys.time() in
  let t1 = integer_serie_stress_test () in
  report "integer serie stress test" t1;
  Printf.printf "Total execution time: %fs\n" (Sys.time() -. t)
