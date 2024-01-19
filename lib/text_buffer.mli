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
    val prev_newline_offset : container -> option int
    val next_newline_offset : container -> option int
    val get_at_cursor : container -> char
    val set_at_cursor : char -> container -> container 
end

module Make (DS : TextEditDataStructure) :
    S with type container := char DS.t


