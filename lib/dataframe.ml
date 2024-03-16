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

end

module Serie = struct

  type t =
    | Source of Ftype.t array
    | Derived of Ftype.t Seq.t

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
end

module IntSet = Set.Make(Int)

type dataframe = Serie.t list * IntSet.t
