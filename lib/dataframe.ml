module Ftype = struct

  type t =
    | Integer of int
    | Numeric of (int * int)
    | String of string

  type f =
    | I_I of (int -> int)
    | I_F of (int -> float)
    | I_S of (int -> string)
    | F_F of (float -> float)
    | F_I of (float -> int)
    | F_S of (float -> string)
    | S_S of (string -> string)
    | S_I of (string -> int)

  let eq a b = (a=b)

  let to_int = function
    | Integer x -> x
    | Numeric(_, x) -> x
    | String _ -> raise @@ Invalid_argument "string cannot be converted"
  let%test _ = to_int (Integer 42) = 42
  let%test _ = to_int @@ Numeric(1, 42) = 42
  let%test _ =
    try to_int @@ String "42" = 51 with
    | Invalid_argument _ -> true
    | _ -> false

  let to_float = function
    | Integer x -> Float.of_int x
    | Numeric(p, x) -> (Float.of_int x) /. (10. ** (Float.of_int p))
    | String _ -> raise (Invalid_argument "string cannot be converted")
  let%test _ = to_float @@ Integer 42 = 42.
  let%test _ = to_float @@ Numeric(1, 42) = 4.2
  let%test _ =
    try to_float @@ String "42" = 5.1 with
    | Invalid_argument _ -> true
    | _ -> false

  let to_string = function
    | Integer x -> Int.to_string x
    | Numeric(p, x) ->
      let divider = 10. ** (Float.of_int p) |> Float.to_int in
      let int_part = x / divider |> Int.to_string in
      let dec_part = x mod divider |> Int.to_string in
      let l = String.length dec_part in
      int_part ^ "." ^ (String.make (p - l) '0') ^ dec_part
    | String s -> s
  let%test _ = to_string @@ Integer 42 = "42"
  let%test _ = to_string @@ Numeric(1, 42) = "4.2"
  let%test _ = to_string @@ Numeric(2, 42) = "0.42"
  let%test _ = to_string @@ Numeric(5, 42) = "0.00042"
  let%test _ = to_string @@ Numeric(15, 42) = "0.000000000000042"
  let%test _ = to_string @@ String "42" = "42"

  let of_int x = Integer x
  let%test _ = of_int 42 = Integer 42

  let num_of_int p x = Numeric(p, x)
  let%test _ = num_of_int 2 4200 = Numeric(2, 4200)

  let num_of_float p x =
    Numeric(p, Float.to_int(Float.round(x *. 10. ** (Float.of_int p))))
  let%test _ = num_of_float 2 42.356 = Numeric(2, 4236)
  let%test _ = num_of_float 2 42.352 = Numeric(2, 4235)

  let of_string s = String s
  let%test _ = of_string "hello" = String "hello"

  let from_int_to_int f = I_I f
  let from_int_to_float f = I_F f
  let from_int_to_str f = I_S f

  let from_float_to_float f = F_F f
  let from_float_to_int f = F_I f
  let from_float_to_str f = F_S f

  let from_str_to_str f = S_S f
  let from_str_to_int f = S_I f

  let map f ftype =
    match f, ftype with
    | I_I f, Integer x -> Integer (f x)
    | I_S f, Integer x -> String (f x)
    | I_I f, Numeric(p, x) -> Numeric(p, f x)
    | I_F f, Numeric(p, x) -> f x |> num_of_float p
    | I_S f, Numeric(_, x) -> String (f x)
    | F_F f, Numeric(p, x) ->
      let x' = f (Float.of_int (x) *. 10. ** -.(Float.of_int p)) in
      num_of_float p x'
    | F_I f, Numeric(p, x) ->
      let x' = f (Float.of_int (x) *. 10. ** -.(Float.of_int p)) in
      Integer x'
    | F_S f, Numeric(p, x) ->
      let x' = f (Float.of_int (x) *. 10. ** -.(Float.of_int p)) in
      String x'
    | S_S f, String s -> String (f s)
    | S_I f, String s -> Integer (f s)
    | _ -> raise (Invalid_argument "function is incompatible")

  let pp_ftype out = function
  | Integer x -> Fmt.pf out "% 10d" x
  | Numeric (p, x) -> Fmt.pf out "% 10f" (to_float @@ Numeric(p, x))
  | String s -> Fmt.pf out "%S" (String.sub s 0 10)

end

module Serie = struct

  type t =
    | Source of Ftype.t array
    | Derived of Ftype.t Seq.t

  let of_array arr =
    Source arr

  let of_int_seq seq =
    let arr = Seq.map Ftype.of_int seq |> Array.of_seq in
    Source arr
  let%test _ =
    List.to_seq [ 1; 2; 3; 4; 5 ]
    |> of_int_seq
       = Source [| Integer 1; Integer 2; Integer 3; Integer 4; Integer 5 |]

  let nums_of_int_seq p seq =
    let arr = Seq.map (Ftype.num_of_int p) seq |> Array.of_seq in
    Source arr
  let%test _ =
    List.to_seq [ 1; 2; 3; 4; 5 ]
    |> nums_of_int_seq 1
       = Source
         [| Numeric(1, 1)
          ; Numeric(1, 2)
          ; Numeric(1, 3)
          ; Numeric(1, 4)
          ; Numeric(1, 5)
         |]

  let nums_of_float_seq p seq =
    let arr = Seq.map (Ftype.num_of_float p) seq |> Array.of_seq in
    Source arr
  let%test _ =
    List.to_seq [ 0.1; 0.2; 0.3; 0.4; 0.5 ]
    |> nums_of_float_seq 1
       = Source
         [| Numeric(1, 1)
          ; Numeric(1, 2)
          ; Numeric(1, 3)
          ; Numeric(1, 4)
          ; Numeric(1, 5)
         |]

  let of_string_seq seq =
    let arr = Seq.map Ftype.of_string seq |> Array.of_seq in
    Source arr
  let%test _ =
    List.to_seq [ "1"; "2"; "3"; "4"; "5" ]
    |> of_string_seq
       = Source
         [| String "1"
          ; String "2"
          ; String "3"
          ; String "4"
          ; String "5"
         |]

  let length = function
    | Source arr -> Array.length arr
    | Derived seq -> Seq.length seq

  let to_seq = function
    | Source arr -> Array.to_seq arr
    | Derived seq -> seq

  let derive f serie =
    let seq =
      match serie with
      | Source arr -> Array.to_seq arr
      | Derived seq -> seq
    in
    Derived (Seq.map (Ftype.map f) seq)

  let merge f left right =
    match left, right with
    | Source arr, Source arr' ->
      let seq_left = Array.to_seq arr in
      let seq_right = Array.to_seq arr' in
      Seq.zip seq_left seq_right
      |> Seq.map (fun (l, r) -> f l r)
    | Source arr, Derived seq_left ->
      let seq_right = Array.to_seq arr in
      Seq.zip seq_left seq_right
      |> Seq.map (fun (l, r) -> f l r)
    | Derived seq_left, Source arr ->
      let seq_right = Array.to_seq arr in
      Seq.zip seq_left seq_right
      |> Seq.map (fun (l, r) -> f l r)
    | Derived seq_left, Derived seq_right ->
      Seq.zip seq_left seq_right
      |> Seq.map (fun (l, r) -> f l r)

  let pp_serie out serie =
    let open Fmt in
    match serie with
    | Source arr ->
      pf out "Source @[<2>[ %a ]@]"
        (array ~sep:semi Ftype.pp_ftype)
        arr
    | Derived s ->
      pf out "Derived @[<2>[ %a ]@]"
        (seq ~sep:semi Ftype.pp_ftype)
        s

  let eq a b =
    Seq.for_all2 ( = ) (to_seq a) (to_seq b)

  let get rowid = function
    | Source arr -> arr.(rowid)
    | Derived seq ->
      Seq.drop rowid seq |> Seq.take 1 |> List.of_seq |> List.hd

end

module IntSet = Set.Make(Int)

type dataframe = Serie.t list * IntSet.t

let of_list = function
  | [] -> ([], IntSet.empty)
  | h :: t ->
    let len = Serie.length h in
    if List.for_all (fun seq -> Serie.length seq = len) t then
      (h::t, Serie.to_seq h |> Seq.mapi (fun i _ -> i) |> IntSet.of_seq)
    else raise @@ Invalid_argument "series are incompatible"

let serie (l, _) n = List.nth_opt l n

let get_row_ids (_, rows) = IntSet.to_seq rows
let get_row rowid (l, _) =
  List.map (Serie.get rowid) l

let append (l, s) ns =
  if Serie.length ns = IntSet.cardinal s then
    (l @ [ns], s)
  else raise @@ Invalid_argument "serie is incompatible"
let ( +: ) = append

let print_df df =
  let rowids = get_row_ids df in
  Seq.iter
    ( fun rowid ->
        Format.printf "@[%a@]@."
          (Fmt.list ~sep:Fmt.sp Ftype.pp_ftype)
          (get_row rowid df)
    )
    rowids
