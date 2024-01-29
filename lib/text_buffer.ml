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
  val left_n : int -> container -> container
  val right_n : int -> container -> container
  val prev_nl_off : container -> (int, int) Either.t
  val next_nl_off : container -> (int, int) Either.t
  val get_at_cursor : container -> char option
  val set_before_cursor : char -> container -> container
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
  let left_n = DS.move_left_n
  let right_n = DS.move_right_n
  let prev_nl_off c = DS.find_prev '\n' c
  let next_nl_off c = DS.find_next '\n' c
  let is_eq c ch = match DS.elem c with None -> false | Some x -> x == ch
  let to_sol c = match prev_nl_off c with Left n -> n | Right n -> n - 1

  let line_length c =
    let new_c = DS.move_left_n (to_sol c) c in
    match next_nl_off new_c with Left n -> n + 1 | Right n -> n

  let up c =
    match prev_nl_off c with
    | Left _ -> c
    | Right n ->
        let new_c = DS.move_left_n n c in
        let ll = line_length new_c in
        if ll > n then DS.move_left_n (ll - n + 1) new_c else new_c

  let down c =
    let ll = line_length c in
    if is_eq c '\n' then
      let nc = right c in
      match next_nl_off nc with
      | Left n | Right n -> DS.move_right_n (min n ll) nc
    else
      match next_nl_off c with
      | Left _ -> c
      | Right n ->
          let new_c = DS.move_right_n (n + 1) c in
          let new_ll = line_length new_c in
          DS.move_right_n (min new_ll (ll - n)) new_c

  let get_at_cursor = DS.elem
  let set_at_cursor ch c = if DS.is_begin c then c else remove c |> insert ch
end
