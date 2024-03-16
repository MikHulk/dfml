module IntSet: sig
  type t
end

module Ftype: sig
  type t =
    | Integer of int
    | Numeric of (int * int)
    | String of string

  val of_int: int -> t
  val num_of_int: int -> int -> t
  val num_of_float: int -> float -> t
  val of_string: string -> t
end

module Serie: sig
  type t =
    | Source of Ftype.t array
    | Derived of Ftype.t Seq.t

  val of_int_seq: int Seq.t -> t
  val nums_of_int_seq: int -> int Seq.t -> t
  val nums_of_float_seq: int -> float Seq.t -> t
  val of_string_seq: string Seq.t -> t
end

type dataframe = Serie.t list * IntSet.t
