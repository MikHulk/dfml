module Column = struct
  type column =
    | IntegerC of int array
    | NumericC of int * int array
    | StringC of string array

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
        string_of_int (x / (10 ** p)) ^ "." ^ string_of_int (x mod (10 ** p))
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

  let set rowid col t =
    match col, t with
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

  let print_column limit width =
    let print f =
      print_string "| ";
      print_ftype width f;
      print_string " |";
      print_newline ()
    in function
    | StringC arr ->
      Seq.iter
        (fun v -> print (String v))
        (Seq.take limit (Array.to_seq arr))
    | IntegerC arr ->
      Seq.iter
        (fun v -> print (Integer v))
        (Seq.take limit (Array.to_seq arr))
    | NumericC (p, arr) ->
      Seq.iter
        (fun v -> print (Numeric (p, v)))
        (Seq.take limit (Array.to_seq arr))

end

open Column
open Column.Ftype

let enumerate s = Seq.mapi (fun i x -> (i, x)) s

let filteri f s = Seq.filter f (enumerate s)

type dataset =
  | Empty
  | Data of int * int * column array

let of_list ll =
  match ll with
    [] -> Empty
  | h::t ->
    let height = length h in
    let width = List.length ll in
    if List.for_all (fun col -> height = length col) t
    then Data (height, width, (Array.of_list ll))
    else raise
        (Invalid_argument "column must have the same size")

let%test _ =
  match of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]])
  with
    Data _ -> true
  | _ -> false

let get ds col row =
  match ds with
    Empty -> None
  | Data (h, w, arr) ->
    if abs col < w && abs row < h
    then
      let col = if col >= 0 then col else w + col
      and row = if row >= 0 then row else h + row
      in Some (Column.get arr.(col) row)
    else None

let%test _ = get (
    of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  ) 2 4 = Some(Integer 10)

let%test _ = get (
    of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  ) (-1) (-1) = Some(Integer 10)

let%test _ = get (
    of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  ) 2 5 = None


let get' ds col row =
  match ds with
    Empty -> raise (Invalid_argument "empty dataset")
  | Data (_, _, arr) ->
    Column.get arr.(col) row

let%test _ = get' (
    of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  ) 2 4 = Integer 10

let%test _ = try
    get' (
    of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  ) 2 5 = Integer (Random.int 100)
  with Invalid_argument _ -> true
     | _ -> false


let get_column ds col =
  match ds with
    Empty -> None
  | Data (_, w, arr) ->
    if col < w
    then
      let col = if col >= 0 then col else w + col
      in Some arr.(col)
    else None

let%test _ =
  get_column (
    of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  ) 2
  = Some(IntegerC [|2;8;3;9;10|])

let%test _ =
  get_column (
    of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  ) (-3)
  = Some(IntegerC [|1;2;3;4;5|])

let%test _ =
  get_column (
    of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  ) 3
  = None


let get_row ds row =
  match ds with
    Empty -> None
  | Data (h, _, arr) ->
    if abs row < h
    then
      let row = if row < 0 then h + row else row in
      Some (Array.map (fun col -> Column.get col row) arr)
    else None

let%test _ =
  get_row (
    of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
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


let select f ds =
  match ds with
    Empty -> Seq.empty
  | Data (_, _, arr) -> filteri f (Array.to_seq arr)

let get_all seq =
  List.rev (Seq.fold_left (fun acc (i, a) -> (i, a) :: acc) [] seq)

let%test _ = get_all (
    select
      (fun (_, a) ->
         Seq.fold_left ( + ) 0
           (Seq.filter_map unwrap_int (to_seq a)) > 8
      )
      (
        of_list (
          List.map
            intcol_of_list
            [[1;2;3;4;5]
            ;[1;1;2;2;2]
            ;[2;8;3;9;10]]
        )
      )
  ) = [(0, IntegerC [|1;2;3;4;5|]);(2, IntegerC [|2;8;3;9;10|])]

let%test _ = get_all (
    select
      (fun _ -> true)
      (
        of_list (
          List.map
            intcol_of_list
            [[1;2;3;4;5]
            ;[1;1;2;2;2]
            ;[2;8;3;9;10]]
        )
      )
  ) = [(0, IntegerC [|1;2;3;4;5|])
      ;(1, IntegerC [|1;1;2;2;2|])
      ;(2, IntegerC [|2;8;3;9;10|])
      ]

let%test _ = get_all (
    select
      (fun _ -> false)
      (
        of_list (
          List.map
            intcol_of_list
            [[1;2;3;4;5]
            ;[1;1;2;2;2]
            ;[2;8;3;9;10]]
        )
      )
  ) = []


let filter col_id f ds =
  match ds with
    Empty -> Seq.empty
  | _ ->
    begin
      match get_column ds col_id with
        Some col ->
        begin
          Seq.map (fun (i, _) -> i) (filteri f (Column.to_seq col))
        end
      | None -> Seq.empty
    end

let%test _ = List.rev (
    Seq.fold_left ( fun acc x -> x::acc) [] (
      filter
        0
        (fun _ -> true)
        (
          of_list (
            List.map
              intcol_of_list
              [[1;2;3;4;5]
              ;[1;1;2;2;2]
              ;[2;8;3;9;10]]
          )
        )
    )
  ) = [0;1;2;3;4]

let%test _ = List.rev (
    Seq.fold_left ( fun acc x -> x::acc) [] (
      filter
        0
        (fun _ -> false)
        (
          of_list (
            List.map
              intcol_of_list
              [[1;2;3;4;5]
              ;[1;1;2;2;2]
              ;[2;8;3;9;10]]
          )
      )
    )
  ) = []

let%test _ = List.rev (
    Seq.fold_left ( fun acc x -> x::acc) [] (
      filter
        0
        (fun (_, field) ->
           match unwrap_int field with
             Some(v) -> v >  2
           | _ -> false
        )
        (
          of_list (
            List.map
              intcol_of_list
              [[1;2;3;4;5]
              ;[1;1;2;2;2]
              ;[2;8;3;9;10]]
          )
        )
    )
  ) = [2;3;4]


let ( *: ) s f =
  fun ds ->
  begin
    let rows = f ds
    in Seq.map (
      fun row ->
        List.rev
          (Seq.fold_left (fun acc (_, col) -> (Column.get col row)::acc) [] (s ds))
    ) rows
  end

let%test _ =
  let ds = of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  in
  let sele = select (fun (i, _) -> i = 0 || i = 1) in
  let filt =
    filter 2
      ( fun (_, field) ->
          match (unwrap_int field) with
            Some v -> v > 5
          | None -> false
      )
  in
  List.of_seq (
    (sele *: filt) ds
  ) = [[Integer 2;Integer 1];[Integer 4;Integer 2];[Integer 5;Integer 2]]

let%test _ =
  let ds = of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  in
  let sele = select (fun (i, _) -> i = 0 || i = 1) in
  let filt = filter 2 (fun _ -> false) in
  List.of_seq (
    (sele *: filt) ds
  ) = []

let%test _ =
  let ds = of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  in
  let sele = select (fun _ -> false) in
  let filt = filter 2
      ( fun (_, f) ->
          match (unwrap_int f) with
            Some v -> v > 5
          | None -> false
      )
  in
  List.of_seq (
    (sele *: filt) ds
  ) = [[];[];[]]


let ( +: ) s f =
  fun ds ->
  begin
    let rows = f ds
    in Seq.map (
      fun (_, col) ->
        List.rev (
          Seq.fold_left (fun acc row -> (Column.get col row)::acc) [] rows)
    ) (s ds)
  end

let%test _ =
  let ds = of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  in
  let sele = select (fun (i, _) -> i = 0 || i = 1) in
  let filt = filter 2
      ( fun (_, f) ->
          match unwrap_int f with
            Some v -> v > 5
          | None -> false
      )
  in
  List.of_seq (
    (sele +: filt) ds
  ) = [[Integer 2;Integer 4;Integer 5];[Integer 1;Integer 2;Integer 2]]

let%test _ =
  let ds = of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  in
  let sele = select (fun (i, _) -> i = 0 || i = 1) in
  let filt = filter 2 (fun _ -> false) in
  List.of_seq (
    (sele +: filt) ds
  ) = [[];[]]


let%test _ =
  let ds = of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  in
  let sele = select (fun _ -> false) in
  let filt = filter 2
      ( fun (_, f) ->
          match unwrap_int f with
            Some v -> v > 5
          | None -> false
      )
  in
  List.of_seq (
    (sele +: filt) ds
  ) = []


let transform s f t ds
  : unit =
  let cols = (s ds) in
  let rows = (f ds) in
  Seq.iter (
    fun rowid ->
      Seq.iter (
        fun (_, col) ->
          set rowid col (map t (Column.get col rowid))
      ) cols
  ) rows

let%test _ =
  let ds = of_list (
      List.map
        intcol_of_list
        [[1;2;3;4;5]
        ;[1;1;2;2;2]
        ;[2;8;3;9;10]]
    )
  in
  let sele = select (fun (i,_) -> i = 1) in
  let filt = filter 2
      ( fun (_, f) ->
          match unwrap_int f with
            Some v -> v > 5
          | None -> false
      )
  in
  transform sele filt (ApplyOnInt (fun x -> x + 2)) ds;
  List.of_seq (
    ((select (fun _ -> true)) +: (filter 0 (fun _ -> true))) ds
  ) =
  [ [Integer 1;Integer 2;Integer 3;Integer 4;Integer 5]
  ; [Integer 1;Integer 3;Integer 2;Integer 4;Integer 4]
  ; [Integer 2;Integer 8;Integer 3;Integer 9;Integer 10]
  ]

let%test _ =
  let ds = of_list
      [ intcol_of_list [1;2;3;4;5]
      ; numcol_of_list 2 [145;156;243;212;265]
      ; strcol_of_list ["foo";"bar";"foobar";"foobarfoo";"barfoobar"]]
  in
  let sele = select (fun (i,_) -> i = 2) in
  let filt = filter 1
      ( fun (_, f) ->
          match unwrap_numeric f with
            Some v -> v > 2.2
          | None -> false
      )
  in
  transform sele filt (ApplyOnStr (fun _ -> "spam")) ds;
  List.of_seq (
    ((select (fun _ -> true)) +: (filter 0 (fun _ -> true))) ds
  ) =
  [ [Integer 1;Integer 2;Integer 3;Integer 4;Integer 5]
  ; [Numeric(2, 145);Numeric(2, 156);Numeric(2, 243);Numeric(2, 212);Numeric(2, 265)]
  ; [String "foo";String "bar";String "spam";String "foobarfoo";String "spam"]
  ]
