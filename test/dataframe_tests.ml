let testable_ftype =
  let open Dataframe.Ftype in
  Alcotest.testable pp_ftype eq

let testable_serie =
  let open Dataframe.Serie in
  Alcotest.testable pp_serie eq


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
    ( Dataframe.serie df 0 );
  Alcotest.(check  (option testable_serie))
    "s1 should remain the same in df"
    (Some s2)
    ( Dataframe.serie df 1 );
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
    ( Dataframe.serie df 0 );
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
    ( Dataframe.serie df 1 );
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
    ( Dataframe.serie df 2 );
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
    ( Dataframe.serie df 0 );
  Alcotest.(check  (option testable_serie))
    "s2 should remains the same in df"
    (Some s2)
    ( Dataframe.serie df 1 );
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
    ( Dataframe.serie df 2 );
  Alcotest.(check  (list int))
    "df should be indexed properly"
    [0; 1; 2; 3; 4]
    (Dataframe.get_row_ids df |> List.of_seq)

let get_fst_row_from_df () =
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
  Alcotest.(check  (list testable_ftype))
    "should return the first row"
    [ Numeric(2, 511);  Numeric(1, 1); Numeric(2, 521) ]
    ( Dataframe.get_row 0 df)

let get_nth_row_from_df () =
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
  Alcotest.(check  (list testable_ftype))
    "should return the 3rd row"
    [ Numeric(2, 732);  Numeric(1, 3); Numeric(2, 762) ]
    ( Dataframe.get_row 2 df)

let get_lst_row_from_df () =
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
  Alcotest.(check  (list testable_ftype))
    "should return the last row"
    [ Numeric(2, 523);  Numeric(1, 5); Numeric(2, 573) ]
    ( Dataframe.get_row 4 df)

let get_rows_from_df_out_of_order () =
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
  Alcotest.(check  (list testable_ftype))
    "should return the last row"
    [ Numeric(2, 523);  Numeric(1, 5); Numeric(2, 573) ]
    ( Dataframe.get_row 4 df);
  Alcotest.(check  (list testable_ftype))
    "should return the first row"
    [ Numeric(2, 511);  Numeric(1, 1); Numeric(2, 521) ]
    ( Dataframe.get_row 0 df);
  Alcotest.(check  (list testable_ftype))
    "should return the 3rd row"
    [ Numeric(2, 732);  Numeric(1, 3); Numeric(2, 762) ]
    ( Dataframe.get_row 2 df);
  Alcotest.(check  (list testable_ftype))
    "should return the first row"
    [ Numeric(2, 511);  Numeric(1, 1); Numeric(2, 521) ]
    ( Dataframe.get_row 0 df)

let append_to_df () =
  let s1 =
    List.to_seq [ 511; 656; 732; 600; 523]
    |> Dataframe.Serie.nums_of_int_seq 2 in
  let s2 =
    List.to_seq [ 1; 2; 3; 4; 5]
    |> Dataframe.Serie.nums_of_int_seq 1 in
  let orig = Dataframe.of_list [s1] in
  let df = Dataframe.(orig +: s2) in
  Alcotest.(check  (option testable_serie))
    "s1 should remain the same in df"
    (Some s1)
    ( Dataframe.serie df 0 );
  Alcotest.(check  (option testable_serie))
    "s1 should remain the same in df"
    (Some s2)
    ( Dataframe.serie df 1 );
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
    ];
    "test Dataframe.get_row", [
      test_case "get first row from df" `Quick get_fst_row_from_df;
      test_case "get nth row from df" `Quick get_nth_row_from_df;
      test_case "get last row from df" `Quick get_lst_row_from_df;
      test_case "get rows from df out of order" `Quick get_rows_from_df_out_of_order;
    ];
    "test append to df", [
      test_case "append to df" `Quick append_to_df
    ]
  ]
