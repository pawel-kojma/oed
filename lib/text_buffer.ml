module type TextEditDataStructure = sig
  type t

  val empty : 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val move_left : 'a t -> 'a t
  val move_right : 'a t -> 'a t
  val insert : 'a -> 'a t -> 'a t
  val remove : 'a t -> 'a t
  val elem : 'a t -> 'a option
end
