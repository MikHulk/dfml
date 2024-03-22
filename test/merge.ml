let test_merge_strings () =
  let left =
    List.to_seq [ "foo"; "bar"; "baz"]
    |> Dataframe.Serie.of_string_seq in
  let right =
    List.to_seq [ "spam"; "eggs"; "spam"]
    |> Dataframe.Serie.of_string_seq in
  let merge_func l r =
    match l, r with
    | Dataframe.Ftype.String s, Dataframe.Ftype.String s' -> s ^ "_" ^ s'
    | _ -> raise (Invalid_argument "no!") in
  let res = Dataframe.Serie.merge merge_func left right |> List.of_seq in
  Alcotest.(check (list string))
    "should return 2 strings concatenated"
    [ "foo_spam"
    ; "bar_eggs"
    ; "baz_spam"
    ]
    res

let test_merge_integers () =
  let left =
    List.to_seq [ 5; 6; 7; 6; 5]
    |> Dataframe.Serie.of_int_seq in
  let right =
    List.to_seq [ 1; 2; 3; 4; 5]
    |> Dataframe.Serie.of_int_seq in
  let merge_func l r =
    match l, r with
    | Dataframe.Ftype.Integer x, Dataframe.Ftype.Integer y -> x * y
    | _ -> raise (Invalid_argument "no!") in
  let res = Dataframe.Serie.merge merge_func left right |> List.of_seq in
  Alcotest.(check (list int))
    "should return x * y"
    [ 5; 12; 21; 24; 25]
    res

let test_merge_numerics () =
  let left =
    List.to_seq [ 511; 656; 732; 600; 523]
    |> Dataframe.Serie.nums_of_int_seq 2 in
  let right =
    List.to_seq [ 1; 2; 3; 4; 5]
    |> Dataframe.Serie.nums_of_int_seq 1 in
  let merge_func l r =
    match l, r with
    | Dataframe.Ftype.Numeric _, Dataframe.Ftype.Numeric _ ->
      Dataframe.Ftype.(to_float l *. to_float r)
    | _ -> raise (Invalid_argument "no!") in
  let res = Dataframe.Serie.merge merge_func left right
            |> Seq.map Float.to_string
            |> List.of_seq in
  Alcotest.(check  (list string))
    "should return x * y"
    [ "0.511"; "1.312"; "2.196"; "2.4"; "2.615"]
    res

let () = let open Alcotest in
  run "Merge serie" [
    "test serie merging", [
      test_case "merge 2 string series" `Quick test_merge_strings;
      test_case "merge 2 integer series" `Quick test_merge_integers;
      test_case "merge 2 numeric series" `Quick test_merge_numerics;
    ]
  ]
