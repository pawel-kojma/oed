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
  val prev_newline_offset : container -> (int, int) Either.t
  val next_newline_offset : container -> (int, int) Either.t
  val get_at_cursor : container -> char
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

  let rec find_newline c acc move =
    match DS.elem c with
    | None -> Either.left acc
    | Some x when x == '\n' -> Either.right acc
    | _ -> find_newline (move c) (acc + 1) move

  let rec apply b f n = if n <= 0 then b else apply (f b) g (n - 1)
  let prev_newline_offset c = find_newline c 0 DS.move_left
  let next_newline_offset c = find_newline c 0 DS.move_right

  let start_of_line c =
    if DS.is_begin c then true
    else
      match DS.move_left c |> DS.elem with None -> false | Some x -> x == '\n'

  let get_at_cursor c = DS.elem c
  let set_at_cursor ch c = DS.remove c |> DS.insert ch c
end
