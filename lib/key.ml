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

type specialI = Enter | Backspace | Up | Down | Left | Right | Escape | Delete | Tab
type normal_keyset = SpecialKeyN of specialN | NonSpecialKeyN of int
type insert_keyset = SpecialKeyI of specialI | NonSpecialKeyI of int
type _ keyset = Normal : normal_keyset keyset | Insert : insert_keyset keyset

let convert_insert = function
  | x when x == Curses.Key.up -> SpecialKeyI Up
  | x when x == Curses.Key.down -> SpecialKeyI Down
  | x when x == Curses.Key.left -> SpecialKeyI Left
  | x when x == Curses.Key.right -> SpecialKeyI Right
  | x when x == Curses.Key.backspace -> SpecialKeyI Backspace
  | x when x == Curses.Key.dc -> SpecialKeyI Delete
  | 9 -> SpecialKeyI Tab
  | 10 -> SpecialKeyI Enter
  | 27 -> SpecialKeyI Escape
  | x -> NonSpecialKeyI x

let convert_normal = function
  | x when x == Curses.Key.up -> SpecialKeyN Up
  | x when x == Curses.Key.down -> SpecialKeyN Down
  | x when x == Curses.Key.left -> SpecialKeyN Left
  | x when x == Curses.Key.right -> SpecialKeyN Right
  | x when x == Curses.Key.backspace -> SpecialKeyN Backspace
  | 10 -> SpecialKeyN Enter
  | 27 -> SpecialKeyN Escape
  | 117 -> SpecialKeyN Undo
  | 114 -> SpecialKeyN Redo
  | 83 -> SpecialKeyN Save
  | 81 -> SpecialKeyN Quit
  | 105 -> SpecialKeyN I
  | x -> NonSpecialKeyN x

let convert : type a. a keyset -> int -> a =
 fun keyset key ->
  match keyset with
  | Normal -> convert_normal key
  | Insert -> convert_insert key
