let it = 100_000_000

let time f =
    let t = Sys.time() in
    let _ = f () in
    Sys.time() -. t
    
let integer_serie_stress_test () =
  let f = fun () ->
    Array.init it (fun _ -> Dataframe.Ftype.Integer (Random.int 100))
    |> Dataframe.Serie.of_array
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_int_to_int ( ( * ) 50) ) in
  time f

let numeric_serie_stress_test () =
  let f = fun () ->
    Array.init it (fun _ -> Dataframe.Ftype.Numeric (2, (Random.int 100)))
    |> Dataframe.Serie.of_array
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_float_to_float cos ) in
  time f

let string_serie_stress_test () =
  let f = fun () ->
    Array.init it (fun _ -> Dataframe.Ftype.String (Random.int 100 |> Int.to_string))
    |> Dataframe.Serie.of_array
    |> Dataframe.Serie.derive
      (Dataframe.Ftype.from_int_to_str Int.to_string) in
  time f


let report title t =
  print_newline () ;
  Printf.printf "%#d %s\n" it title;
  Printf.printf "Execution time: %fs\n" t;
  print_newline ();
  print_string "===========================================================";
  print_newline ()
  

let () =
  let t = Sys.time() in
  let job1 = Domain.spawn integer_serie_stress_test in
  let job2 = Domain.spawn numeric_serie_stress_test in
  let job3 = Domain.spawn string_serie_stress_test in
  let t3 = Domain.join job3 in
  let t2 = Domain.join job2 in
  let t1 = Domain.join job1 in
  report "integer serie stress test" t1;
  report "numeric serie stress test" t2;
  report "string serie stress test" t3;
  print_newline ();
  Printf.printf "Total execution time: %fs\n" (Sys.time() -. t)
