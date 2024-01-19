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

let is_empty (ctx,lst)= ctx == Start && lst == []
let is_begin (ctx, _) = ctx == Start
let is_end (_, lst) = lst == []

let rec move_left_n gb n =
  if n <= 0 then gb else move_left_n (move_left gb) (n - 1)

let rec move_right_n gb n =
  if n <= 0 then gb else move_right_n (move_right gb) (n - 1)
