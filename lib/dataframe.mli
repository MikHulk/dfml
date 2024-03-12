module Column : sig

  module Ftype : sig
    type ftype =
      | Integer of int
      | Numeric of (int * int)
      | String of string

    type application =
      | ApplyOnInt of (int -> int)
      | ApplyOnStr of (string -> string)

    val unwrap_int: ftype -> int option
    val unwrap_numeric: ftype -> float option
    val unwrap_string: ftype -> string option
    val map: application -> ftype -> ftype

  end

  open Ftype

  type column =
    | IntegerC of int array
    | NumericC of int * int array
    | StringC of string array

  type selection = int Seq.t

  val to_seq: column -> ftype Seq.t
  val length: column -> int
  val intcol_of_list: int list -> column
  val numcol_of_list: int -> int list -> column
  val strcol_of_list: string list -> column
  val select: (ftype -> bool) -> column -> selection
  val filter: column -> selection -> ftype Seq.t
  val get: column -> int -> ftype
  val set: column -> int -> ftype -> unit
  val print_column: int -> int -> column -> unit

end

open Column
open Column.Ftype

type dataset =
  | Empty
  | Data of int * int * column array

val of_list: column list -> dataset
val get: dataset -> int -> int -> ftype option
val get_column: dataset -> int -> column option
val get_row: dataset -> int -> ftype array option
