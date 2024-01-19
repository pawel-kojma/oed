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
  val move_left_n : 'a t -> int -> 'a t
  val move_right : 'a t -> 'a t
  val move_right_n : 'a t -> int -> 'a t

  (* insert element BEFORE the cursor *)
  val insert : 'a -> 'a t -> 'a t

  (* remove element BEFORE the cursor *)
  val remove : 'a t -> 'a t

  (* check element at cursor location *)
  val elem : 'a t -> 'a option
end

module type S = sig
  type container

  val create_empty : container
  val build : string -> container
  val decompose : container -> string
  val insert : char -> container -> container
  val remove : container -> container
  val left : container -> container
  val right : container -> container
  val up : container -> container
  val down : container -> container
  val prev_nl_off : container -> int
  val next_nl_off : container -> int
  val get_at_cursor : container -> char option
  val set_at_cursor : char -> container -> container
end

module Make (DS : TextEditDataStructure) = struct
  (* type container = char DS.t *)
  let list_of_string s = List.init (String.length s) (String.get s)
  let string_of_list l = String.of_seq (List.to_seq l)
  let build str = list_of_string str |> DS.of_list
  let decompose c = DS.to_list c |> string_of_list
  let insert = DS.insert
  let remove = DS.remove
  let left = DS.move_left
  let right = DS.move_right
  let create_empty = DS.empty

  let rec find_newline c acc move =
    match DS.elem c with
    | None -> acc
    | Some x when x == '\n' -> acc
    | _ -> find_newline (move c) (acc + 1) move

  let prev_nl_off c = find_newline c 0 left
  let next_nl_off c = find_newline c 0 right
  let is_eq c ch = match DS.elem c with None -> false | Some x -> x == ch

  let start_of_line c =
    if DS.is_begin c then true
    else
      match left c |> DS.elem with
      | None -> failwith "shouldn't be raised"
      | Some x -> x == '\n'

  let end_of_line c = if DS.is_end c then true else right c |> start_of_line

  let line_length c =
    let l1 = next_nl_off c in
    let l2 = right c |> prev_nl_off in
    l1 + l2 + 1

  let to_sol c =
    if start_of_line c then 0
    else if is_eq c '\n' then prev_nl_off (left c)
    else prev_nl_off c

  let to_eol c = if end_of_line c then 0 else next_nl_off c

  let up c =
    let off = to_sol c in
    let new_c = DS.move_left_n c (off + 1) in
    let ll = line_length new_c in
    if ll > off then DS.move_left_n (left new_c) (ll - off) else new_c

  let down c =
    let off = to_eol c in
    let new_c = DS.move_right_n c (off + 1) in
    let ll = line_length new_c in
    if ll > off then DS.move_right_n new_c (ll - off) else new_c

  let get_at_cursor = DS.elem
  let set_at_cursor ch c = remove c |> insert ch
end
