type specialN =
  | Enter
  | Backspace
  | Up
  | Down
  | Left
  | Right
  | Escape
  | Undo
  | Redo
  | Save
  | Quit
  | I
  | Ctrl_c
    | Shift_i
  | Shift_a 


type specialI = Enter | Backspace | Up | Down | Left | Right | Escape | Delete | Tab
type normal_keyset = SpecialKeyN of specialN | NonSpecialKeyN of int
type insert_keyset = SpecialKeyI of specialI | NonSpecialKeyI of int
type _ keyset = Normal : normal_keyset keyset | Insert : insert_keyset keyset

val convert : 'a keyset -> int -> 'a
