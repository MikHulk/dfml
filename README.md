## An example for the record

first we build a dataset with 1 column:

```ocaml
# open Dataframe;;
# let serie =
    List.to_seq
      [ 0.
      ; Float.pi /. 3.
      ; Float.pi /. 2.
      ; 2. *. Float.pi /. 3.
      ; Float.pi
      ]
    |> Serie.nums_of_float_seq 4;;
val serie : Serie.t =
  Dataframe.Serie.Source
   [|Dataframe.Ftype.Numeric (4, 0); Dataframe.Ftype.Numeric (4, 10472);
     Dataframe.Ftype.Numeric (4, 15708); Dataframe.Ftype.Numeric (4, 20944);
     Dataframe.Ftype.Numeric (4, 31416)|]

# let df = of_list [serie];;
val df : dataframe =
  ([Dataframe.Serie.Source
     [|Dataframe.Ftype.Numeric (4, 0); Dataframe.Ftype.Numeric (4, 10472);
       Dataframe.Ftype.Numeric (4, 15708); Dataframe.Ftype.Numeric (4, 20944);
       Dataframe.Ftype.Numeric (4, 31416)|]],
   <abstr>)
```

then 2 series derived from the first one:

```ocaml
# let s1 =
    Serie.derive
      ( Ftype.from_float_to_float cos )
      serie;;
val s1 : Serie.t = Dataframe.Serie.Derived <fun>

# let s2 =
    Serie.derive
      ( Ftype.from_float_to_float sin )
      serie;;
val s2 : Serie.t = Dataframe.Serie.Derived <fun>

```

with `+:` we append serie to the dataframe

```ocaml
# let df = df +: s1 +: s2;;
val df : dataframe =
  ([Dataframe.Serie.Source
     [|Dataframe.Ftype.Numeric (4, 0); Dataframe.Ftype.Numeric (4, 10472);
       Dataframe.Ftype.Numeric (4, 15708); Dataframe.Ftype.Numeric (4, 20944);
       Dataframe.Ftype.Numeric (4, 31416)|];
    Dataframe.Serie.Derived <fun>; Dataframe.Serie.Derived <fun>],
   <abstr>)
```

We can retrieve any row from this df (provided it exists):
```ocaml
# get_row 3 df;;
- : Ftype.t list =
[Dataframe.Ftype.Numeric (4, 20944); Dataframe.Ftype.Numeric (4, -5000);
 Dataframe.Ftype.Numeric (4, 8660)]

# get_row 0 df;;
- : Ftype.t list =
[Dataframe.Ftype.Numeric (4, 0); Dataframe.Ftype.Numeric (4, 10000);
 Dataframe.Ftype.Numeric (4, 0)]

# get_row 5 df;;
Exception: Invalid_argument "index out of bounds".
Raised by primitive operation at Dataframe.Serie.get in file "lib/dataframe.ml", line 217, characters 20-31
Called from Stdlib__List.map in file "list.ml", line 86, characters 15-19
Called from Topeval.load_lambda in file "toplevel/byte/topeval.ml", line 89, characters 4-14

```

print_df allow to display df on the screen:

```ocaml
# print_df df;;
  0.000000   1.000000   0.000000
  1.047200   0.500000   0.866000
  1.570800   0.000000   1.000000
  2.094400  -0.500000   0.866000
  3.141600  -1.000000   0.000000
- : unit = ()
```
