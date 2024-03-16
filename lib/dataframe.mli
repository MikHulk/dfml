module IntSet : sig
  type t
end

module Ftype : sig
  type t =
    | Integer of int
    | Numeric of (int * int)
    | String of string

  val of_int: int -> t
  val num_of_int: int -> int -> t
  val num_of_float: int -> float -> t
  val of_string: string -> t
end

type serie =
  | Source of Ftype.t array
  | Derived of Ftype.t Seq.t
      
type dataframe = serie list * IntSet.t
