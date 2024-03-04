type column =
  | IntegerC of int array
  | NumericC of int * int array
  | StringC of string array

type ftype =
  | Integer of int
  | Numeric of (int * int)
  | String of string

type 'a dataset =
  | Empty
  | Data of int * int * column array


let to_seq (col: column) =
  match col with
    IntegerC arr -> Seq.map (fun v -> Integer v) (Array.to_seq arr)
  | NumericC (p, arr) -> Seq.map (fun v -> Numeric (p, v)) (Array.to_seq arr)
  | StringC arr -> Seq.map (fun v -> String v) (Array.to_seq arr)

let unwrap_int (field: ftype): int option =
  match field with
  | Integer x -> Some(x)
  | Numeric (p, x) -> Some(x / (p * 10))
  | String _ -> None
let%test _ = unwrap_int (Integer 42) = Some 42
let%test _ = unwrap_int (Numeric (1, 42)) = Some 4
let%test _ = unwrap_int (String "42") = None

let integer_of_list ll =
  IntegerC (Array.of_list ll)

let%test _ =
  match integer_of_list [1;2;3;4] with
    IntegerC _ -> true
  | _ -> false

let numeric_of_list precision ll =
  NumericC (precision, Array.of_list ll)

let%test _ =
  match numeric_of_list 2 [156;232;367;432] with
    NumericC (p, arr) -> p = 2 && arr.(1) = 232
  | _ -> false

let string_of_list ll =
  StringC (Array.of_list ll)

let%test _ =
  match string_of_list ["un";"deux";"trois";"quatre"] with
    StringC arr -> arr = [|"un";"deux";"trois";"quatre"|]
  | _ -> false


let enumerate s = Seq.mapi (fun i x -> (i, x)) s


let of_list (ll: 'a list list): 'a dataset =
  match ll with
    [] -> Empty
  | h::t ->
    let height = List.length h in
    let width = List.length ll in
    if List.for_all (fun l -> height = List.length l) t
    then Data (height, width, (Array.of_list (List.map integer_of_list ll)))
    else raise (Invalid_argument "column must have the same size")

let%test _ = match of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] with
    Data _ -> true
  | _ -> false

let get_cell col row =
  match col with
    IntegerC arr -> Integer arr.(row)
  | NumericC (p, arr) -> Numeric (p, arr.(row))
  | StringC arr -> String arr.(row)

let get ds col row =
  match ds with
    Empty -> None
  | Data (h, w, arr) ->
    if col < w && row < h
    then Some (get_cell arr.(col) row)
    else None

let%test _ = get (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 4 = Some(Integer 10)
let%test _ = get (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 5 = None


let get' ds col row =
  match ds with
    Empty -> raise (Invalid_argument "empty dataset")
  | Data (_, _, arr) ->
    get_cell arr.(col) row

let%test _ = get' (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 4 = Integer 10

let%test _ = try
    get' (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 5 = Integer (Random.int 100)
  with Invalid_argument _ -> true
     | _ -> false


let get_column ds col =
  match ds with
    Empty -> None
  | Data (_, w, arr) ->
    if col < w
    then Some arr.(col)
    else None

let%test _ =
  get_column (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2
  = Some(IntegerC [|2;8;3;9;10|])

let%test _ =
  get_column (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 3
  = None


let get_row ds row =
  match ds with
    Empty -> None
  | Data (h, _, arr) ->
    if row < h
    then Some (Array.map (fun col -> get_cell col row) arr)
    else None

let%test _ =
  get_row (
    of_list
      [ [1;2;3;4;5]
      ; [1;1;2;2;2]
      ; [2;8;3;9;10]
      ]
  ) 2 = Some([|Integer 3;Integer 2;Integer 3|])
    
let%test _ =
  get_row (
    of_list
      [ [1;2;3;4;5]
      ; [1;1;2;2;2]
      ; [2;8;3;9;10]
      ]
  ) 2 <> Some([|Integer 1;Integer 1;Integer 2|])
    
let%test _ =
  get_row (
    of_list
      [ [1;2;3;4;5]
      ; [1;1;2;2;2]
      ; [2;8;3;9;10]
      ]
  ) 0 = Some([|Integer 1;Integer 1;Integer 2|])
    
let%test _ = get_row (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 5 = None


let filter_col f s =
  Seq.filter f
    (enumerate s)


let select (f: int * column -> bool) (ds: 'a dataset): (int * column) Seq.t =
  match ds with
    Empty -> Seq.empty
  | Data (_, _, arr) -> filter_col f (Array.to_seq arr)

let get_all (seq: (int * column) Seq.t): (int * column) list =
  List.rev (Seq.fold_left (fun acc (i, a) -> (i, a) :: acc) [] seq)

let%test _ = get_all (
    select
      (fun (_, a) -> Seq.fold_left ( + ) 0 (Seq.filter_map unwrap_int (to_seq a)) > 8)
      (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
  ) = [(0, IntegerC [|1;2;3;4;5|]);(2, IntegerC [|2;8;3;9;10|])]

let%test _ = get_all (
    select
      (fun _ -> true)
      (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
  ) = [(0, IntegerC [|1;2;3;4;5|]);(1, IntegerC [|1;1;2;2;2|]);(2, IntegerC [|2;8;3;9;10|])]

let%test _ = get_all (
    select
      (fun _ -> false)
      (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
  ) = []


let filter (col_id: int) (f: int * ftype -> bool) (ds: 'a dataset): 'a Seq.t =
  match ds with
    Empty -> Seq.empty
  | _ ->
    begin
      match get_column ds col_id with
        Some col ->
        begin
          Seq.map (fun (i, _) -> i) (filter_col f (to_seq col))
        end
      | None -> Seq.empty
    end

let%test _ = List.rev (
    Seq.fold_left ( fun acc x -> x::acc) [] (
      filter
        0
        (fun _ -> true)
        (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
    )
  ) = [0;1;2;3;4]

let%test _ = List.rev (
    Seq.fold_left ( fun acc x -> x::acc) [] (
      filter
        0
        (fun _ -> false)
        (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
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
        (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
    )
  ) = [2;3;4]


let ( *: ) (s: 'a dataset -> (int * column) Seq.t) (f: 'a dataset -> 'a Seq.t) =
  fun (ds: 'a dataset) ->
  begin
    let rows = f ds
    in Seq.map (
      fun row -> List.rev (Seq.fold_left (fun acc (_, col) -> (get_cell col row)::acc) [] (s ds))
    ) rows
  end

let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
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
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
  let sele = select (fun (i, _) -> i = 0 || i = 1) in
  let filt = filter 2 (fun _ -> false) in
  List.of_seq (
    (sele *: filt) ds
  ) = []

let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
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


let ( +: ) (s: 'a dataset -> (int * column) Seq.t) (f: 'a dataset -> 'a Seq.t) =
  fun (ds: 'a dataset) ->
  begin
    let rows = f ds
    in Seq.map (
      fun (_, col) -> List.rev (Seq.fold_left (fun acc row -> (get_cell col row)::acc) [] rows) 
    ) (s ds)
  end

let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
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
      
let%test "fail" = (
    let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
    let sele = select (fun (i, _) -> i = 0 || i = 1) in
    let filt = filter 2 (fun _ -> false) in
    List.of_seq (
      (sele +: filt) ds
    ) = [[];[]]
  )
  
let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
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

let map f col rowid =
  match col with
    IntegerC arr -> arr.(rowid) <- (f arr.(rowid))
  | _ -> ()

let transform
    (s: 'a dataset -> (int * column) Seq.t)
    (f: 'a dataset -> 'a Seq.t)
    (t: 'a -> 'a)
    (ds: 'a dataset)
  : unit =
  let cols = (s ds) in
  let rows = (f ds) in
  Seq.iter (
    fun rowid ->
      Seq.iter (
        fun (_, col) ->
          map t col rowid
      ) cols
  ) rows

let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
  let sele = select (fun (i,_) -> i = 1) in
  let filt = filter 2
      ( fun (_, f) ->
          match unwrap_int f with
            Some v -> v > 5
          | None -> false
      )
  in
  transform sele filt (fun x -> x + 2) ds;
  List.of_seq (
    ((select (fun _ -> true)) +: (filter 0 (fun _ -> true))) ds
  ) =
  [ [Integer 1;Integer 2;Integer 3;Integer 4;Integer 5]
  ; [Integer 1;Integer 3;Integer 2;Integer 4;Integer 4]
  ; [Integer 2;Integer 8;Integer 3;Integer 9;Integer 10]
  ]
