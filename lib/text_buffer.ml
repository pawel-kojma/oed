module type TextEditDataStructure = sig
  type 'a t

  (* create new instance *)
  val empty : 'a t

  (* predicates *)
  val is_empty : 'a t -> bool
  val is_begin : 'a t -> bool
  val is_end : 'a t -> bool

  (* build from or decompose to list *)
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list

  (* move cursor around *)
  val move_left : 'a t -> 'a t
  val move_left_n : int -> 'a t -> 'a t
  val move_right : 'a t -> 'a t
  val move_right_n : int -> 'a t -> 'a t

  (* insert element BEFORE the cursor *)
  val insert : 'a -> 'a t -> 'a t

  (* remove element BEFORE the cursor *)
  val remove : 'a t -> 'a t

  (* check element at cursor location *)
  val elem : 'a t -> 'a option

  (* return offset of next/previous element in collection *)
  val find_prev : 'a -> 'a t -> (int, int) Either.t
  val find_next : 'a -> 'a t -> (int, int) Either.t
end

module type S = sig
  type t
  type cords = int * int

  val of_cords : cords -> int * int
  val create_empty : t
  val build : string -> t
  val decompose : t -> string
  val insert : char -> t -> t * cords
  val remove : t -> t * cords
  val left : t -> t * cords
  val right : t -> t * cords
  val up : t -> t * cords
  val down : t -> t * cords
  val prev_nl_off : t -> (int, int) Either.t
  val next_nl_off : t -> (int, int) Either.t
  val get_at_cursor : t -> char option
  val get_before_cursor : t -> char option
  val next_line : t -> string option
end

module Make (DS : TextEditDataStructure) = struct
  type t = char DS.t * int DS.t * int
  type cords = int * int

  let inc i =
    match DS.elem i with
    | None -> failwith "inc"
    | Some a -> DS.remove i |> DS.insert (a + 1)

  let dec i =
    match DS.elem i with
    | None -> failwith "dec"
    | Some a -> DS.remove i |> DS.insert (a - 1)

  let app_cords ((c, i, s) : t) : t * cords =
    let rec _app_cords i n =
      if DS.is_begin i then n - 1 else _app_cords (DS.move_left i) (n + 1)
    in
    ((c, i, s), (_app_cords i 0, s))

  let elem_sure c =
    match DS.elem c with None -> failwith "elem_sure" | Some x -> x

  let is_eq c ch = match DS.elem c with None -> false | Some x -> x == ch
  let line_length i = match DS.elem i with None -> 0 | Some n -> n
  let list_of_string s = List.init (String.length s) (String.get s)
  let string_of_list l = String.of_seq (List.to_seq l)
  let of_cords (x, y) = (x, y)
  let create_empty = (DS.empty, DS.of_list [ 0 ], 0)

  let build str =
    let rec _build i lst =
      match lst with
      | [] -> i
      | x :: xs when x == '\n' -> _build (DS.insert 0 i) xs
      | _ :: xs -> _build (inc i) xs
    in
    let lstr = list_of_string str in
    (DS.of_list lstr, _build (DS.of_list [ 0 ]) lstr, 0)

  let decompose (c, _, _) = DS.to_list c |> string_of_list

  let insert ch (c, i, s) =
    if ch == '\n' then
      let ll = line_length i in
      (DS.insert ch c, DS.remove i |> DS.insert s |> DS.insert (ll - s), 0)
      |> app_cords
    else (DS.insert ch c, inc i, s + 1) |> app_cords

  let remove (c, i, s) =
    match DS.elem c with
    | None -> (c, i, s) |> app_cords
    | Some ch when ch == '\n' ->
        let lp = line_length (DS.move_left i) and ll = line_length i in
        (DS.remove c, DS.remove i |> DS.remove |> DS.insert (ll + lp), lp)
        |> app_cords
    | Some _ -> (DS.remove c, dec i, s - 1) |> app_cords

  let left (c, i, s) =
    let nc = DS.move_left c in
    if is_eq c '\n' then (c, i, s) |> app_cords else (nc, i, s - 1) |> app_cords

  let right (c, i, s) =
    let nc = DS.move_right c in
    if is_eq nc '\n' then (c, i, s) |> app_cords
    else (nc, i, s + 1) |> app_cords

  let prev_nl_off (c, _, _) = DS.find_prev '\n' c
  let next_nl_off (c, _, _) = DS.find_next '\n' c

  let up (c, i, s) =
    if DS.move_left i |> DS.is_begin then (c, i, s) |> app_cords
    else
      let lp = line_length (DS.move_left i) in
      let off, ns = if s > lp then (s + 1, lp) else (lp, s) in
      (DS.move_left_n off c, DS.move_left i, ns) |> app_cords

  let down (c, i, s) =
    if DS.is_end i then (c, i, s) |> app_cords
    else
      let ll = line_length i and ln = line_length (DS.move_right i) in
      let off, ns = if s > ln then (ll - s + ln, ln) else (ll, s) in
      (DS.move_right_n off c, DS.move_right i, ns) |> app_cords

  let next_line (c, i, s) =
    if DS.is_end i then None
    else
      let rec _next_line acc n g =
        if n > 0 then _next_line (elem_sure g :: acc) (n - 1) (DS.move_right g)
        else Some (string_of_list (List.rev acc))
      in
      _next_line []
        (line_length (DS.move_right i))
        (DS.move_right_n (line_length i - s) c)

  let get_at_cursor (c, _, _) = DS.elem c

  let get_before_cursor (c, _, _) =
    if DS.is_begin c then None else DS.move_left c |> DS.elem
end
