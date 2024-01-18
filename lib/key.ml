type special = Enter | Backspace | Up | Down | Left | Right | Escape
type keytype = SpecialKey of special | NonSpecialKey of int

let convert = function
  | x when x == Curses.Key.up -> SpecialKey Up
  | x when x == Curses.Key.down -> SpecialKey Down
  | x when x == Curses.Key.left -> SpecialKey Left
  | x when x == Curses.Key.right -> SpecialKey Right
  | x when x == Curses.Key.backspace -> SpecialKey Backspace
  | 10 -> SpecialKey Enter
  | 27 -> SpecialKey Escape
  | x -> NonSpecialKey x
