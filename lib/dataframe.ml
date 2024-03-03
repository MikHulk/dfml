type 'a dataset =
  | Empty
  | Data of int * int * 'a array array

let enumerate s = Seq.mapi (fun i x -> (i, x)) s

let of_list (ll: 'a list list): 'a dataset =
  match ll with
    [] -> Empty
  | h::t ->
    let height = List.length h in
    let width = List.length ll in
    if List.for_all (fun l -> height = List.length l) t
    then Data (height, width, (Array.of_list (List.map Array.of_list ll)))
    else raise (Invalid_argument "column must have the same size")


let%test _ = match of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] with
    Data _ -> true
  | _ -> false

let get ds col row =
  match ds with
    Empty -> None
  | Data (h, w, arr) ->
    if col < w && row < h
    then Some arr.(col).(row)
    else None


let%test _ = get (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 4 = Some(10)


let%test _ = get (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 5 = None


let get' ds col row =
  match ds with
    Empty -> raise (Invalid_argument "empty dataset")
  | Data (_, _, arr) ->
    arr.(col).(row)


let%test _ = get' (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 4 = 10


let%test _ = try
    get' (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 5 = 10
  with Invalid_argument _ -> true
     | _ -> false


let get_column ds col =
  match ds with
    Empty -> None
  | Data (_, w, arr) ->
    if col < w
    then Some arr.(col)
    else None


let%test _ = get_column (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 = Some([|2;8;3;9;10|])


let%test _ = get_column (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 3 = None


let get_row ds row =
  match ds with
    Empty -> None
  | Data (h, _, arr) ->
    if row < h
    then Some (Array.map (fun arr' -> Array.get arr' row) arr)
    else None


let%test _ = get_row (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 = Some([|3;2;3|])


let%test _ = get_row (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 2 <> Some([|1;1;2|])


let%test _ = get_row (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 0 = Some([|1;1;2|])


let%test _ = get_row (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]]) 5 = None


let filter_col f s =
  Seq.filter f
    (enumerate s)


let to_list (rows: 'a Seq.t): 'a list =
  List.rev (Seq.fold_left (fun acc x -> (List.rev x)::acc) [] rows)


let select (f: int * 'a array -> bool) (ds: 'a dataset): (int * 'a array) Seq.t =
  match ds with
    Empty -> Seq.empty
  | Data (_, _, arr) -> filter_col f (Array.to_seq arr)


let get_all (seq: (int * 'a array) Seq.t): (int * 'a array) list =
  List.rev (Seq.fold_left (fun acc (i, a) -> (i, a) :: acc) [] seq)


let%test _ = get_all (
    select
      (fun (_, a) -> Seq.fold_left ( + ) 0 (Array.to_seq a) > 8)
      (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
  ) = [(0, [|1;2;3;4;5|]);(2, [|2;8;3;9;10|])]


let%test _ = get_all (
    select
      (fun _ -> true)
      (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
  ) = [(0, [|1;2;3;4;5|]);(1, [|1;1;2;2;2|]);(2, [|2;8;3;9;10|])]


let%test _ = get_all (
    select
      (fun _ -> false)
      (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
  ) = []


let filter (col_id: int) (f: 'a * 'a -> bool) (ds: 'a dataset): 'a Seq.t =
  match ds with
    Empty -> Seq.empty
  | _ ->
    begin
      match get_column ds col_id with
        Some col ->
        begin
          Seq.map (fun (i, _) -> i) (filter_col f (Array.to_seq col))
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
        (fun (_, v) -> v > 2)
        (of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]])
    )
  ) = [2;3;4]


let ( *: ) (s: 'a dataset -> (int * 'a array) Seq.t) (f: 'a dataset -> 'a Seq.t) =
  fun (ds: 'a dataset) ->
  begin
    let rows = f ds
    in Seq.map (
      fun row -> Seq.fold_left (fun acc (_, a) -> a.(row)::acc) [] (s ds)
    ) rows
  end


let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
  let sele = select (fun (i, _) -> i = 0 || i = 1) in
  let filt = filter 2 (fun (_, v) -> v > 5) in
  to_list (
    (sele *: filt) ds
  ) = [[2;1];[4;2];[5;2]]


let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
  let sele = select (fun (i, _) -> i = 0 || i = 1) in
  let filt = filter 2 (fun _ -> false) in
  to_list (
    (sele *: filt) ds
  ) = []


let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
  let sele = select (fun _ -> false) in
  let filt = filter 2 (fun (_, v) -> v > 5) in
  to_list (
    (sele *: filt) ds
  ) = [[];[];[]]


let ( +: ) (s: 'a dataset -> (int * 'a array) Seq.t) (f: 'a dataset -> 'a Seq.t) =
  fun (ds: 'a dataset) ->
  begin
    let rows = f ds
    in Seq.map (
      fun (_, col) -> Seq.fold_left (fun acc row -> col.(row)::acc) [] rows 
    ) (s ds)
  end


let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
  let sele = select (fun (i, _) -> i = 0 || i = 1) in
  let filt = filter 2 (fun (_, v) -> v > 5) in
  to_list (
    (sele +: filt) ds
  ) = [[2;4;5];[1;2;2]]


let%test "fail" = (
    let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
    let sele = select (fun (i, _) -> i = 0 || i = 1) in
    let filt = filter 2 (fun _ -> false) in
    to_list (
      (sele +: filt) ds
    ) = [[];[]]
  )


let%test _ =
  let ds = of_list [[1;2;3;4;5];[1;1;2;2;2];[2;8;3;9;10]] in
  let sele = select (fun _ -> false) in
  let filt = filter 2 (fun (_, v) -> v > 5) in
  to_list (
    (sele +: filt) ds
  ) = []

