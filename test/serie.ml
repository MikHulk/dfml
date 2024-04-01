let ftype_testable =
  let open Dataframe.Ftype in
  Alcotest.testable pp_ftype eq

let (>>) f g x = g(f(x))

let test_derive_int_to_int () =
  let serie' =
    List.to_seq [ 1; 2; 3; 4; 5; 6 ]
    |> Dataframe.Serie.of_int_seq 6
    |> Dataframe.Serie.derive
      (Dataframe.Ftype.from_int_to_int (( + ) 2 >> ( * ) 8)) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return perform `8(x + 2)` from Integer serie"
    [ Integer 24; Integer 32; Integer 40; Integer 48; Integer 56; Integer 64 ]
    res
    
let test_derive_int_to_string () =
  let serie' =
    List.to_seq [ 13; 12; 34; 42; 51 ]
    |> Dataframe.Serie.of_int_seq 5
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_int_to_str Int.to_string ) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return a String serie"
    [String "13"; String "12"; String "34"; String "42"; String "51"]
    res

let test_derive_string_to_int () =
  let serie' =
    List.to_seq [ "a"; "ab"; "abc"; "abcd"; "abcde" ]
    |> Dataframe.Serie.of_string_seq 5
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_str_to_int String.length ) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return an Integer serie"
    [ Integer 1; Integer 2; Integer 3; Integer 4; Integer 5 ]
    res
    
let test_derive_numerics_to_numerics_process_int_returns_int () =
  let serie' =
    List.to_seq [ 13; 12; 34; 42; 51 ]
    |> Dataframe.Serie.nums_of_int_seq 5 1
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_int_to_int (( + ) 100) ) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return the Numeric serie + 100"
    [ Numeric(1, 113)
    ; Numeric(1, 112)
    ; Numeric(1, 134)
    ; Numeric(1, 142)
    ; Numeric(1, 151)
    ]
    res
    
let test_derive_numerics_to_numerics_process_int_returns_float () =
  let serie' =
    List.to_seq [ 13; 12; 34; 42; 51 ]
    |> Dataframe.Serie.nums_of_int_seq 5 1
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_int_to_float
          (Float.of_int >> (Fun.flip ( /. ) 2.))
      ) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return the Numeric serie /2"
    [ Numeric(1, 65)
    ; Numeric(1, 60)
    ; Numeric(1, 170)
    ; Numeric(1, 210)
    ; Numeric(1, 255)
    ]
    res
    
let test_derive_numerics_to_numerics_process_int_returns_string () =
  let serie' =
    List.to_seq [ 13; 12; 34; 42; 51 ]
    |> Dataframe.Serie.nums_of_int_seq 5 1
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_int_to_str Int.to_string  ) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return the Numeric serie as string"
    [ String "13"
    ; String "12"
    ; String "34"
    ; String "42"
    ; String "51"
    ]
    res
    
let test_derive_numerics_to_numerics_process_float () =
  let serie' =
    List.to_seq [ 1300; 1200; 3400; 4200; 5100 ]
    |> Dataframe.Serie.nums_of_int_seq 5 3
    |> Dataframe.Serie.derive ( Dataframe.Ftype.from_float_to_float cos ) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return the numeric serie aplyinng cos"
    [ Numeric(3, 267)
    ; Numeric(3, 362)
    ; Numeric(3, -967)
    ; Numeric(3, -490)
    ; Numeric(3, 378)
    ]
    res
    
let test_derive_numerics_to_integers_process_float () =
  let serie' =
    List.to_seq [ 15; 14; 33; 42; 51 ]
    |> Dataframe.Serie.nums_of_int_seq 5 1
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_float_to_int (( *. ) 10. >> Float.to_int)) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return the numeric *10 and converted to integers"
    [ Integer 15
    ; Integer 14
    ; Integer 33
    ; Integer 42
    ; Integer 51
    ]
    res
    
let test_derive_numerics_to_strings_process_float () =
  let serie' =
    List.to_seq [ 15; 14; 33; 42; 51 ]
    |> Dataframe.Serie.nums_of_int_seq 5 1
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_float_to_str Float.to_string ) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return the Numeric converted to string"
    [ String "1.5"
    ; String "1.4"
    ; String "3.3"
    ; String "4.2"
    ; String "5.1"
    ]
    res
    
let test_derive_strings_to_strings () =
  let serie' =
    List.to_seq [ "foo"; "bar"; "baz"]
    |> Dataframe.Serie.of_string_seq 3
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_str_to_str  ( ( ^ ) "foo")) in
  let res =
    match serie' with
    | Derived seq ->
      List.of_seq seq
    | _ -> [] in
  Alcotest.(check (list ftype_testable))
    "should return the strings with concat"
    [ String "foofoo"
    ; String "foobar"
    ; String "foobaz"
    ]
    res

let get_from_serie () =
  let serie =
    List.to_seq [ "foo"; "bar"; "baz"]
    |> Dataframe.Serie.of_string_seq 3 in
  Alcotest.(check ftype_testable)
    "should return the nth element from serie"
    (String "baz")
    (Dataframe.Serie.get 2 serie)

let get_from_seq_serie () =
  let serie =
    List.to_seq [ "foo"; "bar"; "baz"]
    |> Dataframe.Serie.of_string_seq 3
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_str_to_str  ( ( ^ ) "foo")) in
  Alcotest.(check ftype_testable)
    "should return the nth element from serie"
    (String "foobaz")
    (Dataframe.Serie.get 2 serie)

let get_first_from_seq_serie () =
  let serie =
    List.to_seq [ "foo"; "bar"; "baz"]
    |> Dataframe.Serie.of_string_seq 3
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_str_to_str  ( ( ^ ) "foo")) in
  Alcotest.(check ftype_testable)
    "should return the nth element from serie"
    (String "foofoo")
    (Dataframe.Serie.get 0 serie)

let get_from_seq_serie_out_of_order () =
  let serie =
    List.to_seq [ "foo"; "bar"; "baz"]
    |> Dataframe.Serie.of_string_seq 3
    |> Dataframe.Serie.derive
      ( Dataframe.Ftype.from_str_to_str  ( ( ^ ) "foo")) in
  Alcotest.(check ftype_testable)
    "should return the nth element from serie"
    (String "foobaz")
    (Dataframe.Serie.get 2 serie);
  Alcotest.(check ftype_testable)
    "should return the nth element from serie"
    (String "foofoo")
    (Dataframe.Serie.get 0 serie)

let () = let open Alcotest in
  run "Serie" [
    "test serie derivation", [
      test_case "derive integers to integers" `Quick test_derive_int_to_int;
      test_case "derive integers to strings" `Quick test_derive_int_to_string;
      test_case
        "derive numerics to numerics processing as int returning int"
        `Quick test_derive_numerics_to_numerics_process_int_returns_int;
      test_case
        "derive numerics to numerics processing as int returning float"
        `Quick test_derive_numerics_to_numerics_process_int_returns_float;
      test_case
        "derive numerics to numerics processing as int returning string"
        `Quick test_derive_numerics_to_numerics_process_int_returns_string;
      test_case
        "derive numerics to numerics processing as float"
        `Quick test_derive_numerics_to_numerics_process_float;
      test_case
        "derive numerics to integers processing as float"
        `Quick test_derive_numerics_to_integers_process_float;
      test_case
        "derive numerics to stgs processing as float"
        `Quick test_derive_numerics_to_strings_process_float;
      test_case
        "derive strings to strings"
        `Quick test_derive_strings_to_strings;
      test_case "derive strings to integers" `Quick test_derive_string_to_int;
    ];
    "test serie get row", [
      test_case "get nth element from serie" `Quick get_from_serie;
      test_case "get nth element from derived serie" `Quick get_from_seq_serie;
      test_case "get first element from derived serie"
        `Quick get_first_from_seq_serie;
      test_case "get nth element from derived serie out of order"
        `Quick get_from_seq_serie_out_of_order;
    ]
  ]
