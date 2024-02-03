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
  val nth_next_line : int -> t -> string option
  val get_lines : int -> t -> string list
  val debug_view : t -> char list * int list * int
end

module Make (DS : TextEditDataStructure) : S
