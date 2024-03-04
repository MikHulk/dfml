type ftype =
  | Integer of int
  | Numeric of (int * int)
  | String of string

type column =
  | IntegerC of int array
  | NumericC of int * int array
  | StringC of string array

type 'a dataset =
  | Empty
  | Data of int * int * column array

val of_list: int list list -> int dataset
val get: 'a dataset -> int -> int -> ftype option
val get_column: 'a dataset -> int -> column option
val get_row: 'a dataset -> int -> ftype array option
val select: (int * column -> bool) -> 'a dataset -> (int * column) Seq.t
val get_all: (int * column) Seq.t -> (int * column) list
val filter: int -> (int * ftype -> bool) -> int dataset -> int Seq.t
val ( *: ):
  (int dataset -> (int * column) Seq.t)
  -> (int dataset -> int Seq.t)
  -> (int dataset -> ftype list Seq.t)
val ( +: ):
  (int dataset -> (int * column) Seq.t)
  -> (int dataset -> int Seq.t)
  -> (int dataset -> ftype list Seq.t)
val transform:
  (int dataset -> (int * column) Seq.t)
  -> (int dataset -> int Seq.t)
  -> (int -> int)
  -> int dataset
  -> unit
