type 'a context = Start | Right of 'a context * 'a
type 'a gap_buffer
type 'a t = 'a gap_buffer

val empty : 'a gap_buffer
val is_empty : 'a gap_buffer -> bool
val is_begin : 'a gap_buffer -> bool
val is_end : 'a gap_buffer -> bool
val of_list : 'a list -> 'a gap_buffer
val to_list : 'a gap_buffer -> 'a list
val move_left : 'a gap_buffer -> 'a gap_buffer
val move_right : 'a gap_buffer -> 'a gap_buffer
val insert : 'a -> 'a gap_buffer -> 'a gap_buffer
val remove : 'a gap_buffer -> 'a gap_buffer
val elem : 'a gap_buffer -> 'a option
val move_left_n : int -> 'a gap_buffer -> 'a gap_buffer
val move_right_n : int -> 'a gap_buffer -> 'a gap_buffer
val find_prev : 'a -> 'a gap_buffer -> (int, int) Either.t
val find_next : 'a -> 'a gap_buffer -> (int, int) Either.t
