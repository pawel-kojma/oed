type 'a context = Start | Right of 'a context * 'a
type 'a gap_buffer = 'a context * 'a list

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
