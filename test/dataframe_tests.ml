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
    "s1 should remains the same in df"
    ( Dataframe.get_serie df 0 )
    (Some s1);
  Alcotest.(check  (option testable_serie))
    "s1 should remains the same in df"
    ( Dataframe.get_serie df 1 )
    (Some s2);
  Alcotest.(check  (list int))
    "df should be indexed properly"
    (Dataframe.get_row_ids df |> List.of_seq)
    [0; 1; 2; 3; 4]

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
    "serie should returns cosines from serie"
    ( Dataframe.get_serie df 0 )
    (Some serie);
  Alcotest.(check  (option testable_serie))
    "s1 should returns cosines from serie"
    ( Dataframe.get_serie df 1 )
    (Some s1);
  Alcotest.(check  (option testable_serie))
    "s2 should returns sines from serie"
    ( Dataframe.get_serie df 2 )
    (Some s2);
  Alcotest.(check  (list int))
    "df should be indexed properly"
    (Dataframe.get_row_ids df |> List.of_seq)
    [0; 1; 2; 3; 4]


let () = let open Alcotest in
  run "Dataframe" [
    "test Dataframe.of_list", [
      test_case "build from sources" `Quick df_of_sources;
      test_case "build from source and make computation on"
        `Quick df_of_source_and_computation;
    ]
  ]
