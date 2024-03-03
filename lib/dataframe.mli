type 'a dataset =
  | Empty
  | Data of int * int * 'a array array
val of_list: 'a list list -> 'a dataset
val get: 'a dataset -> int -> int -> 'a option
val get_column: 'a dataset -> int -> 'a array option
val get_row: 'a dataset -> int -> 'a array option
val to_list: 'a list Seq.t -> 'a list list
val select: (int * 'a array -> bool) -> 'a dataset -> (int * 'a array) Seq.t
val get_all: (int * 'a array) Seq.t -> (int * 'a array) list
val filter: int -> (int * int -> bool) -> int dataset -> int Seq.t
val ( *: ):
  (int dataset -> (int * int array) Seq.t)
  -> (int dataset -> int Seq.t)
  -> (int dataset -> int list Seq.t)
val ( +: ):
  (int dataset -> (int * int array) Seq.t)
  -> (int dataset -> int Seq.t)
  -> (int dataset -> int list Seq.t)
