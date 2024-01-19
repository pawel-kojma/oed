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
  val prev_newline_offset : container -> option int
  val next_newline_offset : container -> option int
  val get_at_cursor : container -> char
  val set_at_cursor : char -> container -> container
end

module Make (DS : TextEditDataStructure) : S with type container := char DS.t
