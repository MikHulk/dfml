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

end

type serie =
  | Source of Ftype.t array
  | Derived of Ftype.t Seq.t

module IntSet = Set.Make(Int)

type dataframe = serie list * IntSet.t
