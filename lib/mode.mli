open Key

type mode = Normal | Insert

type editor = {
  mutable edit_mode : mode;
  insert_action : editor -> Curses.window -> keytype -> unit;
  normal_action : editor -> Curses.window -> keytype -> unit;
}

val normal_mode_action : editor -> Curses.window -> keytype -> unit
val insert_mode_action : editor -> Curses.window -> keytype -> unit
