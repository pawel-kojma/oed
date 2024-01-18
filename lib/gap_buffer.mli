type 'a context = Start | Right of 'a context * 'a
type 'a gap_buffer = 'a context * 'a list

val empty : 'a gap_buffer
val of_list : 'a list -> 'a gap_buffer
val to_list : 'a gap_buffer -> 'a list
val move_left : 'a gap_buffer -> 'a gap_buffer
val move_right : 'a gap_buffer -> 'a gap_buffer
val insert : 'a -> 'a gap_buffer -> 'a gap_buffer
val remove : 'a gap_buffer -> 'a gap_buffer
val elem : 'a gap_buffer -> 'a option
