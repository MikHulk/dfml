## An example for the record

first we build a dataset with 3 columns:

```ocaml
let s = Dataframe.of_list
    [ Dataframe.Column.strcol_of_list
        ["un"; "un"; "un"; "deux"; "deux"; "trois"]
    ; Dataframe.Column.numcol_of_list 2
        [321;10023;6789;932;25;6]
    ; Dataframe.Column.intcol_of_list
        [1;2;3;4;5;6]
    ]
```
result:

```ocaml
val s : Dataframe.dataset =
  Dataframe.Data (6, 3,
   [|Dataframe.Column.StringC [|"un"; "un"; "un"; "deux"; "deux"; "trois"|];
     Dataframe.Column.NumericC (2, [|321; 10023; 6789; 932; 25; 6|]);
     Dataframe.Column.IntegerC [|1; 2; 3; 4; 5; 6|]|])
```

then a function for filtering:

```ocaml
let f = function
  | Dataframe.Column.Ftype.Integer x -> x > 2 && x < 5
  | _ -> false
```

`get_row` allows us to work on a given column:

```ocaml
let col = Dataframe.get_column s 2;;
```

```ocaml
val col : Dataframe.Column.column option =
  Some (Dataframe.Column.IntegerC [|1; 2; 3; 4; 5; 6|])
```

now we build a selector function on the given column

```ocaml
let sele = Dataframe.Column.select f (Option.get (Dataframe.get_column s 2));;
```

sele is a `Seq.t` on the rowid which match the selector function on the column `col`

We can now obtain all the elements in each column corresponding to this subset:

```ocaml
let res = Dataframe.Column.filter (Option.get col) sele;;
List.rev (Seq.fold_left (Fun.flip List.cons) [] res);;
```

result:

```ocaml
[Dataframe.Column.Ftype.Integer 3; Dataframe.Column.Ftype.Integer 4]
```
