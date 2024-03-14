module Column = struct
  type column =
    | IntegerC of int array
    | NumericC of int * int array
    | StringC of string array

  type selection = int Seq.t

  module Ftype = struct
    type ftype =
      | Integer of int
      | Numeric of (int * int)
      | String of string

    type application =
      | ApplyOnInt of (int -> int)
      | ApplyOnStr of (string -> string)

    let rec pow a = function
      | 0 -> 1
      | 1 -> a
      | n ->
        let b = pow a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a)

    let%test _ = pow 2 2 = 4

    let ( ** ) a = pow a

    let unwrap_int = function
      | Integer x -> Some(x)
      | Numeric (p, x) -> Some(x / (10 ** p))
      | String _ -> None

    let%test _ = unwrap_int (Integer 42) = Some 42
    let%test _ = unwrap_int (Numeric (1, 42)) = Some 4
    let%test _ = unwrap_int (String "42") = None

    let unwrap_numeric = function
      | Integer x -> Some(float_of_int x)
      | Numeric (p, x) -> Some(float_of_int x /. float_of_int (10 ** p))
      | String _ -> None

    let unwrap_string = function
      | String s -> Some s
      | _ -> None


    let to_str fd =
      match fd with
        Integer x -> string_of_int x
      | Numeric (p, x) ->
        string_of_int (x / (10 ** p))
        ^ "."
        ^ string_of_int (x mod (10 ** p))
      | String s -> s

    let%test _ = to_str (Integer 42) = "42"
    let%test _ = to_str (Numeric (1, 422)) = "42.2"
    let%test _ = to_str (String "42") = "42"

    let print_ftype width v =
      let s = to_str v in
      let l = String.length s in
      if l = width
      then
        print_string s
      else if l > width
      then print_string ((String.sub s 0 (width - 3)) ^ "...")
      else
        match v with
          String _ ->
          print_string s;
          for _= 0 to (width - l) do
            print_string " "
          done
        | _ ->
          for _= 0 to (width - l) do
            print_string " "
          done;
          print_string s

    let map f v =
      match f, v with
        ApplyOnInt f, Integer x -> Integer (f x)
      | ApplyOnInt f, Numeric (p, x) -> Numeric (p, f x)
      | ApplyOnStr f, String s -> String (f s)
      | _, _ -> raise (Invalid_argument "bad operation")

  end

  open Ftype

  let enumerate s = Seq.mapi (fun i x -> (i, x)) s

  let to_seq = function
    | IntegerC arr -> Seq.map (fun v -> Integer v) (Array.to_seq arr)
    | NumericC (p, arr) -> Seq.map (fun v -> Numeric (p, v)) (Array.to_seq arr)
    | StringC arr -> Seq.map (fun v -> String v) (Array.to_seq arr)

  let length = function
    | IntegerC arr -> Array.length arr
    | NumericC (_, arr) -> Array.length arr
    | StringC arr -> Array.length arr


  let intcol_of_list ll =
    IntegerC (Array.of_list ll)

  let%test _ =
    match intcol_of_list [1;2;3;4] with
      IntegerC _ -> true
    | _ -> false


  let numcol_of_list precision ll =
    NumericC (precision, Array.of_list ll)

  let%test _ =
    match numcol_of_list 2 [156;232;367;432] with
      NumericC (p, arr) -> p = 2 && arr.(1) = 232
    | _ -> false


  let strcol_of_list ll =
    StringC (Array.of_list ll)

  let%test _ =
    match  strcol_of_list ["un";"deux";"trois";"quatre"] with
      StringC arr -> arr = [|"un";"deux";"trois";"quatre"|]
    | _ -> false

  let get col row =
    match col with
      IntegerC arr -> Integer arr.(row)
    | NumericC (p, arr) -> Numeric (p, arr.(row))
    | StringC arr -> String arr.(row)

  let select (f: ftype -> bool) (col: column): int Seq.t =
    let sigma (i, x) = if f x then Some(i) else None
    in Seq.filter_map sigma (enumerate (to_seq col))

  let%test _ =
    intcol_of_list [1;2;3;4]
    |> select (
      fun fd ->
        match fd with
          Integer x -> x > 2
        | _ -> false
    )
    |> List.of_seq
    = [2;3]

  let filter col sel =
    match col with
      IntegerC arr -> Seq.map (fun i -> Integer arr.(i)) sel
    | NumericC (p, arr) -> Seq.map (fun i -> Numeric (p, arr.(i))) sel
    | StringC arr -> Seq.map (fun i -> String arr.(i)) sel

  let%test _ =
    let col = intcol_of_list [1;2;3;4] in
    let sel = col |> select (
        fun fd ->
          match fd with
            Integer x -> x > 2
          | _ -> false
      )
    in
    filter col sel
    |> List.of_seq
    = [Integer 3;Integer 4]

  let set col rowid value =
    match col, value with
      NumericC (p, arr), Numeric (p', x) ->
      arr.(rowid) <-
        let d = p - p' in
        if d < 0
        then x / (10 ** (p' - p))
        else x * (10 ** (p - p'))
    | IntegerC arr, Integer x ->
      arr.(rowid) <- x
    | StringC arr, String s ->
      arr.(rowid) <- s
    | _, _ -> raise (Invalid_argument "bad operation")


  let update col f rowid =
    get col rowid |> map f |> set col rowid

  let%test _ =
    let col = intcol_of_list [1;2;3;4] in
    let f = ApplyOnInt (fun x -> x * 2) in
    update col f 2;
    col = IntegerC [|1;2;6;4|]

  let%test _ =
    let col = numcol_of_list 1 [1;2;3;4] in
    let f = ApplyOnInt (fun x -> x * 2) in
    update col f 2;
    col = NumericC (1, [|1;2;6;4|])

  let%test _ =
    let col = strcol_of_list ["1";"2";"3";"4"] in
    let f = ApplyOnStr (fun s ->  s ^ "2") in
    update col f 2;
    col = StringC [|"1";"2";"32";"4"|]


  let update_rows col f rowids =
    Seq.iter (update col f) rowids

  let%test _ =
    let col = intcol_of_list [1;2;3;4] in
    let f = ApplyOnInt (fun x -> x * 2) in
    update_rows col f (List.to_seq [2;3]);
    col = IntegerC [|1;2;6;8|]

  let%test _ =
    let col = numcol_of_list 1 [1;2;3;4] in
    let f = ApplyOnInt (fun x -> x * 2) in
    update_rows col f (List.to_seq [2;3]);
    col = NumericC (1, [|1;2;6;8|])

  let%test _ =
    let col = strcol_of_list ["1";"2";"3";"4"] in
    let f = ApplyOnStr (fun s ->  s ^ "2") in
    update_rows col f (List.to_seq [2;3]);
    col = StringC [|"1";"2";"32";"42"|]


  let print_column limit width =
    let print field =
      print_string "| ";
      print_ftype width field;
      print_string " |";
      print_newline ()
    in function
    | StringC arr ->
      Seq.take limit (Array.to_seq arr)
      |> Seq.iter (fun v -> print (String v))
    | IntegerC arr ->
      Seq.take limit (Array.to_seq arr)
      |> Seq.iter (fun v -> print (Integer v))
    | NumericC (p, arr) ->
      Seq.take limit (Array.to_seq arr)
      |> Seq.iter (fun v -> print (Numeric (p, v)))

end

module Dataset = struct
  open Column
  open Column.Ftype

  type dataset = int * int * column array

  let of_list ll =
    match ll with
      [] -> (0, 0, [||])
    | h::t ->
      let height = length h in
      let width = List.length ll in
      if List.for_all (fun col -> height = length col) t
      then (height, width, (Array.of_list ll))
      else Invalid_argument "column must have the same size"
           |> raise

  let%test _ =
    (
      [[1;2;3;4;5]
      ;[1;1;2;2;2]
      ;[2;8;3;9;10]]
      |> List.map intcol_of_list
      |> of_list
    ) =
    ( 5
    , 3
    , [|IntegerC [|1;2;3;4;5|]
      ; IntegerC [|1;1;2;2;2|]
      ; IntegerC [|2;8;3;9;10|]|]
    )
    
  let get (h, w, arr) col row =
    if abs col < w && abs row < h
    then
      let col = if col >= 0 then col else w + col
      and row = if row >= 0 then row else h + row
      in Some (Column.get arr.(col) row)
    else None

  let%test _ =
    get (
      [[1;2;3;4;5]
      ;[1;1;2;2;2]
      ;[2;8;3;9;10]]
      |> List.map intcol_of_list
      |> of_list
    ) 2 4 = Some(Integer 10)

  let%test _ =
    get (
      [[1;2;3;4;5]
      ;[1;1;2;2;2]
      ;[2;8;3;9;10]]
      |> List.map intcol_of_list
      |> of_list
    ) (-1) (-1) = Some(Integer 10)

  let%test _ =
    get (
      [[1;2;3;4;5]
      ;[1;1;2;2;2]
      ;[2;8;3;9;10]]
      |> List.map intcol_of_list
      |> of_list
    ) 2 5 = None


  let get' (_, _, arr) col row =
    Column.get arr.(col) row

  let%test _ =
    get' (
      [[1;2;3;4;5]
      ;[1;1;2;2;2]
      ;[2;8;3;9;10]]
      |> List.map intcol_of_list
      |> of_list
    ) 2 4 = Integer 10

  let%test _ = try
      get' (
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
        |> List.map intcol_of_list
        |> of_list
      ) 2 5 = Integer (Random.int 100)
    with
      Invalid_argument _ -> true
    | _ -> false


  let get_column (_, w, arr) col =
    if col < w
    then
      let col = if col >= 0 then col else w + col
      in Some arr.(col)
    else None

  let%test _ =
    get_column (
      [[1;2;3;4;5]
      ;[1;1;2;2;2]
      ;[2;8;3;9;10]]
      |> List.map intcol_of_list
      |> of_list
    ) 2
    = Some(IntegerC [|2;8;3;9;10|])

  let%test _ =
    get_column (
      [[1;2;3;4;5]
      ;[1;1;2;2;2]
      ;[2;8;3;9;10]]
      |> List.map intcol_of_list
      |> of_list
    ) (-3)
    = Some(IntegerC [|1;2;3;4;5|])

  let%test _ =
    get_column (
      [[1;2;3;4;5]
      ;[1;1;2;2;2]
      ;[2;8;3;9;10]]
      |> List.map intcol_of_list
      |> of_list
    ) 3
    = None


  let get_columns ds cols =
    Seq.filter_map (get_column ds) cols

  let%test _ =
    let
      ds = of_list (
        List.map
          intcol_of_list
          [[1;2;3;4;5]
          ;[1;1;2;2;2]
          ;[2;8;3;9;10]]
      )
    in List.to_seq [0;1;2]
       |> get_columns ds
       |> List.of_seq =
          [ IntegerC [|1;2;3;4;5|]
          ; IntegerC [|1;1;2;2;2|]
          ; IntegerC [|2;8;3;9;10|]
          ]

  let%test _ =
    let
      ds = of_list (
        [ intcol_of_list [1;2;3;4;5]
        ; numcol_of_list 1 [1;1;2;2;2]
        ; strcol_of_list ["2";"8";"3";"9";"10"]
        ]
      )
    in List.to_seq [0;1;2]
       |> get_columns ds
       |> List.of_seq =
          [ IntegerC [|1;2;3;4;5|]
          ; NumericC (1, [|1;1;2;2;2|])
          ; StringC [|"2";"8";"3";"9";"10"|]
          ]

  let%test _ =
    let
      ds = of_list (
        [ intcol_of_list [1;2;3;4;5]
        ; numcol_of_list 1 [1;1;2;2;2]
        ; strcol_of_list ["2";"8";"3";"9";"10"]
        ]
      )
    in get_columns ds Seq.empty
       |> List.of_seq = []

  let%test _ =
    let
      ds = of_list (
        [ intcol_of_list [1;2;3;4;5]
        ; numcol_of_list 1 [1;1;2;2;2]
        ; strcol_of_list ["2";"8";"3";"9";"10"]
        ]
      )
    in get_columns ds (List.to_seq [2])
     |> List.of_seq = [StringC [|"2";"8";"3";"9";"10"|]]

  let%test _ =
    let
      ds = of_list (
        [ intcol_of_list [1;2;3;4;5]
        ; numcol_of_list 1 [1;1;2;2;2]
        ; strcol_of_list ["2";"8";"3";"9";"10"]
        ]
      )
    in get_columns ds (List.to_seq [2;0])
     |> List.of_seq =
        [ StringC [|"2";"8";"3";"9";"10"|]
        ; IntegerC [|1;2;3;4;5|]
        ]


  let get_row (h, _, arr) row =
    if abs row < h
    then
      let row = if row < 0 then h + row else row in
      Some (Array.map (fun col -> Column.get col row) arr)
    else None

  let%test _ =
    get_row (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
      |> of_list
    ) 2 = Some([|Integer 3;Integer 2;Integer 3|])

  let%test _ =
    get_row (
      of_list
        [ intcol_of_list [1;2;3;4;5]
        ; numcol_of_list 1 [1;1;2;2;2]
        ; strcol_of_list ["2";"8";"3";"9";"10"]
        ]
    ) 2 = Some([|Integer 3;Numeric (1, 2);String "3"|])

  let%test _ =
    get_row (
      of_list
        [ intcol_of_list [1;2;3;4;5]
        ; numcol_of_list 1 [1;1;2;2;2]
        ; strcol_of_list ["2";"8";"3";"9";"10"]
        ]
    ) (-3) = Some([|Integer 3;Numeric (1, 2);String "3"|])

  let%test _ =
    get_row (
      of_list (
        List.map
          intcol_of_list
          [[1;2;3;4;5]
          ;[1;1;2;2;2]
          ;[2;8;3;9;10]]
      )
    ) 0 = Some([|Integer 1;Integer 1;Integer 2|])

  let%test _ = get_row (
      of_list (
        List.map
          intcol_of_list
          [[1;2;3;4;5]
          ;[1;1;2;2;2]
          ;[2;8;3;9;10]]
      )
    ) 5 = None

end

open Column
open Dataset

type dataframe = dataset * int Seq.t * int Seq.t


let columns_to_seq (ds, cols, rows) =
  Seq.map
    ( fun col ->
        Seq.filter_map (get ds col) rows
        |> List.of_seq
    ) cols

let%test _ =
  let df =
    ( of_list
        ( List.map
            intcol_of_list
            [[1;2;3;4;5]
            ;[1;1;2;2;2]
            ;[2;8;3;9;10]]
        )
    , List.to_seq [0;1]
    , List.to_seq [0;2;4]
    )
  in columns_to_seq df
     |> List.of_seq =
        [[Integer 1;Integer 3;Integer 5]
        ;[Integer 1;Integer 2;Integer 2]]


let rows_to_seq (ds, cols, rows) =
  Seq.map
    ( fun row ->
        Seq.filter_map (fun col -> get ds col row) cols
        |> List.of_seq
    ) rows

let%test _ =
  let df =
    ( of_list
        ( List.map
            intcol_of_list
            [[1;2;3;4;5]
            ;[1;1;2;2;2]
            ;[2;8;3;9;10]]
        )
    , List.to_seq [0;1]
    , List.to_seq [0;2;4]
    )
  in rows_to_seq df
     |> List.of_seq =
        [[Integer 1;Integer 1]
        ;[Integer 3;Integer 2]
        ;[Integer 5;Integer 2]]
