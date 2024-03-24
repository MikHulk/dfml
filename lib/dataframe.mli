module IntSet: Set.S with type elt = int

module Ftype: sig

  type t =
    | Integer of int
    | Numeric of (int * int)
    | String of string

  val pp_ftype: Format.formatter -> t -> unit

  val eq: t -> t -> bool

  val to_int: t -> int
  val to_float: t -> float
  val to_string: t -> string

  val of_int: int -> t
  val num_of_int: int -> int -> t
  val num_of_float: int -> float -> t
  val of_string: string -> t

  type f

  val from_int_to_int: (int -> int) -> f
  val from_int_to_float: (int -> float) -> f
  val from_int_to_str: (int -> string) -> f
  val from_float_to_float: (float -> float) -> f
  val from_float_to_int: (float -> int) -> f
  val from_float_to_str: (float -> string) -> f
  val from_str_to_str: (string -> string) -> f
  val from_str_to_int: (string -> int) -> f

  val map: f -> t -> t

end

module Serie: sig

  type t =
    | Source of Ftype.t array
    | Derived of Ftype.t Seq.t

  val of_int_seq: int Seq.t -> t
  val nums_of_int_seq: int -> int Seq.t -> t
  val nums_of_float_seq: int -> float Seq.t -> t
  val of_string_seq: string Seq.t -> t

  val length: t -> int
  val to_seq: t -> Ftype.t Seq.t

  val derive: Ftype.f -> t -> t
  val merge: (Ftype.t -> Ftype.t -> 'a) -> t -> t -> 'a Seq.t

  val eq: t -> t -> bool

  val pp_serie: Format.formatter -> t -> unit

end


type dataframe = Serie.t list * IntSet.t

val of_list: Serie.t list -> dataframe
val get_serie: dataframe -> int -> Serie.t option
val get_row_ids: dataframe -> int Seq.t
