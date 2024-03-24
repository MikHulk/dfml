let pprint_ftype ppf = function
  | Dataframe.Ftype.Integer x -> Fmt.pf ppf "Integer %d" x
  | Numeric (p, x) -> Fmt.pf ppf "Numeric (%d, %d)" p x
  | String s -> Fmt.pf ppf "String \"%s\"" s

let pprint_serie ppf serie =
  let open Fmt in
  match serie with
  | Dataframe.Serie.Source arr ->
    pf ppf "Source @[<2>[ %a ]@]"
      (array ~sep:semi pprint_ftype)
      arr
  | Derived s ->
    pf ppf "Derived @[<2>[ %a ]@]"
      (seq ~sep:semi pprint_ftype)
      s

let serie_eq a b =
  Seq.for_all2 ( = ) (Dataframe.Serie.to_seq a) (Dataframe.Serie.to_seq b)

let testable_serie = Alcotest.testable pprint_serie serie_eq


let df_of_sources () =
  let s1 =
    List.to_seq [ 511; 656; 732; 600; 523]
    |> Dataframe.Serie.nums_of_int_seq 2 in
  let s2 =
    List.to_seq [ 1; 2; 3; 4; 5]
    |> Dataframe.Serie.nums_of_int_seq 1 in
  let df = Dataframe.of_list [s1; s2] in
  Alcotest.(check  (option testable_serie))
    "s1 should remain the same in df"
    (Some s1)
    ( Dataframe.get_serie df 0 );
  Alcotest.(check  (option testable_serie))
    "s1 should remain the same in df"
    (Some s2)
    ( Dataframe.get_serie df 1 );
  Alcotest.(check  (list int))
    "df should be indexed properly"
    [0; 1; 2; 3; 4]
    (Dataframe.get_row_ids df |> List.of_seq)

let df_of_source_and_computation () =
  let serie =
    List.to_seq
      [ 0.
      ; Float.pi /. 3.
      ; Float.pi /. 2.
      ; 2. *. Float.pi /. 3.
      ; Float.pi
      ]
    |> Dataframe.Serie.nums_of_float_seq 4 in
  let s1 =
    Dataframe.Serie.derive
      ( Dataframe.Ftype.from_float_to_float cos )
      serie in
  let s2 =
    Dataframe.Serie.derive
      ( Dataframe.Ftype.from_float_to_float sin )
      serie in
  let df = Dataframe.of_list [ serie; s1; s2 ] in
  Alcotest.(check  (option testable_serie))
    "serie should remain the same in the df"
    (Some serie)
    ( Dataframe.get_serie df 0 );
  Alcotest.(check  (option testable_serie))
    "s1 should returns cosines from serie"
    (Some
       ( Dataframe.Serie.Derived
           ( List.to_seq 
               [ Dataframe.Ftype.Numeric (4, 10000)
               ; Numeric (4, 5000)
               ; Numeric (4, 0)
               ; Numeric (4, -5000)
               ; Numeric (4, -10000)
               ]
           )
       )
    )
    ( Dataframe.get_serie df 1 );
  Alcotest.(check  (option testable_serie))
    "s2 should returns sines from serie"
    (Some 
       ( Dataframe.Serie.Derived
           ( List.to_seq 
               [ Dataframe.Ftype.Numeric (4, 0)
               ; Numeric (4, 8660)
               ; Numeric (4, 10000)
               ; Numeric (4, 8660)
               ; Numeric (4, 0)
               ]
           )
       )
    )
    ( Dataframe.get_serie df 2 );
  Alcotest.(check  (list int))
    "df should be indexed properly"
    [0; 1; 2; 3; 4]
    (Dataframe.get_row_ids df |> List.of_seq)


let merge_2_sources () =
  let open Dataframe.Serie in
  let open Dataframe.Ftype in
  let s1 =
    List.to_seq [ 511; 656; 732; 600; 523]
    |> nums_of_int_seq 2 in
  let s2 =
    List.to_seq [ 1; 2; 3; 4; 5]
    |> nums_of_int_seq 1 in
  let f l r =
    match l, r with
    | Numeric(pr, x), Numeric(pl, y) ->
      Numeric( pr
             , x +
               ( Float.to_int
                   ( Float.of_int y
                     *. (10. ** (Float.of_int pl))
                   )
               )
             )
    | _ -> raise @@ Invalid_argument "function incompatible" in
  let s3 = Derived (merge f s1 s2) in
  let df = Dataframe.of_list [s1; s2; s3] in
  Alcotest.(check  (option testable_serie))
    "s1 should remains the same in df"
    (Some s1)
    ( Dataframe.get_serie df 0 );
  Alcotest.(check  (option testable_serie))
    "s2 should remains the same in df"
    (Some s2)
    ( Dataframe.get_serie df 1 );
  Alcotest.(check  (option testable_serie))
    "s3 should returns the addition from S1 and s2 elements"
    (Some 
       ( Dataframe.Serie.Derived
           ( List.to_seq 
               [ Dataframe.Ftype.Numeric (2, 521)
               ; Numeric (2, 676)
               ; Numeric (2, 762)
               ; Numeric (2, 640)
               ; Numeric (2, 573)
               ]
           )
       )
    )
    ( Dataframe.get_serie df 2 );
  Alcotest.(check  (list int))
    "df should be indexed properly"
    [0; 1; 2; 3; 4]
    (Dataframe.get_row_ids df |> List.of_seq)


let () = let open Alcotest in
  run "Dataframe" [
    "test Dataframe.of_list", [
      test_case "build from sources" `Quick df_of_sources;
      test_case "build from source and make computation on"
        `Quick df_of_source_and_computation;
      test_case "build from 2 sources and a merge"
        `Quick merge_2_sources;
    ]
  ]
