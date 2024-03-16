module IntSet = Set.Make(Int)

type ftype =
  | Integer of int
  | Numeric of (int * int)
  | String of string
           
type serie =
  | Source of ftype array
  | Derived of ftype Seq.t
      
type dataframe = serie list * IntSet.t
