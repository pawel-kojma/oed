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
  | Ctrl_c

type specialI = Enter | Backspace | Up | Down | Left | Right | Escape
type normal_keyset = SpecialKeyN of specialN | NonSpecialKeyN of int
type insert_keyset = SpecialKeyI of specialI | NonSpecialKeyI of int
type _ keyset = Normal : normal_keyset keyset | Insert : insert_keyset keyset

val convert : 'a keyset -> int -> 'a

