type special = Enter | Backspace | Up | Down | Left | Right | Escape
type keytype = SpecialKey of special | NonSpecialKey of int

val convert : int -> keytype
