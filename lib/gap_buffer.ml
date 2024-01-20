type 'a context = Start | Right of 'a context * 'a
type 'a gap_buffer = 'a context * 'a list
type 'a t = 'a gap_buffer

let empty = (Start, [])
let of_list lst = (Start, lst)

let rec to_list (ctx, lst) =
  match ctx with Start -> lst | Right (ctx, el) -> to_list (ctx, el :: lst)

let move_left (ctx, lst) =
  match ctx with Start -> (ctx, lst) | Right (ctx, el) -> (ctx, el :: lst)

let move_right (ctx, lst) =
  match lst with [] -> (ctx, lst) | hd :: tl -> (Right (ctx, hd), tl)

let insert a (ctx, lst) = (Right (ctx, a), lst)
let elem (_, lst) = match lst with [] -> None | hd :: _ -> Some hd

let remove (ctx, lst) =
  match ctx with Start -> (ctx, lst) | Right (ctx, _) -> (ctx, lst)

let is_empty (ctx, lst) = ctx == Start && lst == []
let is_begin (ctx, _) = ctx == Start
let is_end (_, lst) = lst == []

let rec move_left_n n gb =
  if n <= 0 then gb else move_left_n (n - 1) (move_left gb)

let rec move_right_n n gb =
  if n <= 0 then gb else move_right_n (n - 1) (move_right gb)

let find_prev e (ctx, _) =
  let rec _find_prev acc e ctx =
    match ctx with
    | Start -> Either.left acc
    | Right (_, el) when el == e -> acc + 1 |> Either.right
    | Right (ctx, _) -> _find_prev (acc + 1) e ctx
  in
  _find_prev 0 e ctx

let find_next e (_, lst) =
  let rec _find_next acc e lst =
    match lst with
    | [] -> Either.left acc
    | hd :: _ when hd == e -> acc + 1 |> Either.right
    | _ :: tl -> _find_next (acc + 1) e tl
  in
  if lst == [] then Either.left 0 else _find_next 0 e (List.tl lst)
