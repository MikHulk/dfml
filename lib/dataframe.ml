module Ftype = struct

  type t =
    | Integer of int
    | Numeric of (int * int)
    | String of string

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
