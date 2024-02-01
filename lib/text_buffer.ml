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

  (* insert element BEFORE/AT the cursor *)
  val insert_at : 'a -> 'a t -> 'a t
  val insert_before : 'a -> 'a t -> 'a t

  (* remove element BEFORE/AT the cursor *)
  val remove_at : 'a t -> 'a t
  val remove_before : 'a t -> 'a t

  (* copy n next elements starting AT cursor *)
  val copy_n : int -> 'a t -> 'a t

  (* check element BEFORE/AT cursor location *)
  val elem_at : 'a t -> 'a option
  val elem_before : 'a t -> 'a option
  val find_prev : 'a -> 'a t -> (int, int) Either.t
  val find_next : 'a -> 'a t -> (int, int) Either.t
end

module type S = sig
  type t

  val to_cords : t -> int * int
  val create_empty : t
  val build : string -> t
  val decompose : t -> string
  val insert : char -> t -> t
  val remove : t -> t
  val left : t -> t
  val right : t -> t
  val up : t -> t
  val down : t -> t
  val prev_nl_off : t -> (int, int) Either.t
  val next_nl_off : t -> (int, int) Either.t
  val get_at_cursor : t -> char option
  val get_before_cursor : t -> char option
  val next_line : t -> string option
  val get_line : t -> string
  val debug_view : t -> char list * int list * int
end

module Make (DS : TextEditDataStructure) = struct
  type t = char DS.t * int DS.t * int

  let debug_view (c, i, s) = (DS.to_list c, DS.to_list i, s)

  let inc i =
    match DS.elem_at i with
    | None -> failwith "inc"
    | Some a -> DS.remove_at i |> DS.insert_at (a + 1)

  let dec i =
    match DS.elem_at i with
    | None -> failwith "dec"
    | Some a -> DS.remove_at i |> DS.insert_at (a - 1)

  let to_cords (_, i, s) =
    let rec _app_cords i n =
      if DS.is_begin i then n else _app_cords (DS.move_left i) (n + 1)
    in
    (_app_cords i 0, s)

  let line_length i = match DS.elem_at i with None -> 0 | Some n -> n
  let list_of_string s = List.init (String.length s) (String.get s)
  let string_of_list l = String.of_seq (List.to_seq l)
  let create_empty = (DS.empty, DS.of_list [ 0 ], 0)
  let rec go_begin c = if DS.is_begin c then c else go_begin (DS.move_left c)

  let build str =
    let rec _build i lst =
      match lst with
      | [] -> i
      | x :: xs when x == '\n' -> _build (DS.move_right i |> DS.insert_at 0) xs
      | _ :: xs -> _build (inc i) xs
    in
    let lstr = list_of_string str in
    (DS.of_list lstr, go_begin (_build (DS.of_list [ 0 ]) lstr), 0)

  let decompose (c, _, _) = DS.to_list c |> string_of_list

  let insert ch (c, i, s) =
    if ch == '\n' then
      let ll = line_length i in
      ( DS.insert_before ch c,
        DS.remove_at i |> DS.insert_before s |> DS.insert_at (ll - s),
        0 )
    else (DS.insert_before ch c, inc i, s + 1)

  let remove (c, i, s) =
    match DS.elem_before c with
    | None -> (c, i, s)
    | Some ch when ch == '\n' ->
        let lp = line_length (DS.move_left i) and ll = line_length i in
        ( DS.remove_before c,
          DS.remove_at i |> DS.remove_before |> DS.insert_at (ll + lp),
          lp )
    | Some _ -> (DS.remove_before c, dec i, s - 1)

  let left (c, i, s) =
    match DS.elem_before c with
    | None -> (c, i, s)
    | Some ch when ch == '\n' -> (c, i, s)
    | Some _ -> (DS.move_left c, i, s - 1)

  let right (c, i, s) =
    match DS.elem_at c with
    | None -> (c, i, s)
    | Some ch when ch == '\n' -> (c, i, s)
    | Some _ -> (DS.move_right c, i, s + 1)

  let prev_nl_off (c, _, _) = DS.find_prev '\n' c
  let next_nl_off (c, _, _) = DS.find_next '\n' c

  let up (c, i, s) =
    if DS.is_begin i then (c, i, s)
    else
      let lp = line_length (DS.move_left i) in
      let off, ns = if s > lp then (s + 1, lp) else (lp + 1, s) in
      (DS.move_left_n off c, DS.move_left i, ns)

  let down (c, i, s) =
    if DS.move_right i |> DS.is_end then (c, i, s)
    else
      let ll = line_length i and ln = line_length (DS.move_right i) in
      let off, ns = if s > ln then (ll - s + ln + 1, ln) else (ll + 1, s) in
      (DS.move_right_n off c, DS.move_right i, ns)

  let next_line (c, i, s) =
    if DS.move_right i |> DS.is_end then None
    else
      let ll = line_length i and ln = DS.move_right i |> line_length in
      Some
        (DS.copy_n ln (DS.move_right_n (ll - s + 1) c)
        |> DS.to_list |> string_of_list)

  let get_line (c, i, s) =
    let ll = line_length i in
    DS.copy_n ll (DS.move_left_n s c) |> DS.to_list |> string_of_list

  let get_at_cursor (c, _, _) = DS.elem_at c
  let get_before_cursor (c, _, _) = DS.elem_before c
end
