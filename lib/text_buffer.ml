module type TextEditDataStructure = sig
  type 'a t

  val empty : 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val move_left : 'a t -> 'a t
  val move_right : 'a t -> 'a t
  val insert : 'a -> 'a t -> 'a t
  val remove : 'a t -> 'a t
  val elem : 'a t -> 'a option
end

module type S = sig
  type container
    
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

  let up c =
    match prev_newline_offset c with
    | Left _ -> c
    | Right off ->
        let new_c = apply c DS.move_left (off + 1) in
        let line_len = prev_newline_offset new_c in
        if line_len > off then apply new_c DS.move_left (line_len - off)
        else new_c

  let down c =
    match next_newline_offset c with
    | Left _ -> c
    | Right off ->
        let new_c = apply c DS.move_right off in
        let line_len = next_newline_offset new_c in
        if line_len > off then apply new_c DS.move_right (line_len - off)
        else new_c

  let get_at_cursor c = DS.elem c
  let set_at_cursor ch c = DS.remove c |> DS.insert ch c
end
