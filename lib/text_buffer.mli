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

module Make (DS : TextEditDataStructure) : S
