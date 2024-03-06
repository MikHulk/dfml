module Column : sig

  type ftype =
    | Integer of int
    | Numeric of (int * int)
    | String of string

  type column =
    | IntegerC of int array
    | NumericC of int * int array
    | StringC of string array

  type application =
    | ApplyOnInt of (int -> int)
    | ApplyOnStr of (string -> string)

  val to_seq: column -> ftype Seq.t
  val length: column -> int
  val unwrap_int: ftype -> int option
  val unwrap_numeric: ftype -> float option
  val unwrap_string: ftype -> string option
  val intcol_of_list: int list -> column
  val numcol_of_list: int -> int list -> column
  val strcol_of_list: string list -> column
  val get: column -> int -> ftype
    
end


type dataset =
  | Empty
  | Data of int * int * Column.column array

val of_list: Column.column list -> dataset
val get: dataset -> int -> int -> Column.ftype option
val get_column: dataset -> int -> Column.column option
val get_row: dataset -> int -> Column.ftype array option
val select: (int * Column.column -> bool) -> dataset -> (int * Column.column) Seq.t
val get_all: (int * Column.column) Seq.t -> (int * Column.column) list
val filter: int -> (int * Column.ftype -> bool) -> dataset -> int Seq.t

val ( *: ):
  (dataset -> (int * Column.column) Seq.t)
  -> (dataset -> int Seq.t)
  -> (dataset -> Column.ftype list Seq.t)

val ( +: ):
  (dataset -> (int * Column.column) Seq.t)
  -> (dataset -> int Seq.t)
  -> (dataset -> Column.ftype list Seq.t)

val transform:
  (dataset -> (int * Column.column) Seq.t)
  -> (dataset -> int Seq.t)
  -> Column.application
  -> dataset
  -> unit
