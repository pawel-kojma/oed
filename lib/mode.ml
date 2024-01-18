open Key

type mode = Normal | Insert

type editor = {
  mutable edit_mode : mode;
  insert_action : editor -> Curses.window -> keytype -> unit;
  normal_action : editor -> Curses.window -> keytype -> unit;
}

let insert_mode_action editor window key =
  match key with
  | SpecialKey key -> (
      let y, x = Curses.getyx window in
      let maxy, maxx = Curses.getmaxyx window in
      match key with
      | Backspace ->
          assert (Curses.mvwdelch window y (x - 1));
          ()
      | Enter when y + 1 >= maxy ->
          assert (Curses.scroll window);
          assert(Curses.move y 0);
          ()
      | Enter when y + 1 < maxy ->
          assert (Curses.addstr "\n");
          ()
      | Up when y - 1 >= 0 ->
          assert (Curses.move (y - 1) x);
          ()
      | Down when y + 1 < maxy ->
          assert (Curses.move (y + 1) x);
          ()
      | Left when x - 1 >= 0 ->
          assert (Curses.move y (x - 1));
          ()
      | Right when x + 1 < maxx ->
          assert (Curses.move y (x + 1));
          ()
      | Escape ->
          editor.edit_mode <- Normal;
          assert (Curses.move (maxy - 1) 0);
          Curses.clrtoeol ();
          assert (Curses.addstr "--NORMAL--");
          assert (Curses.move y x);
          ()
      | _ -> ())
  | NonSpecialKey key ->
      assert (Curses.addch key);
      ()

let normal_mode_action editor window key =
  match key with
  | SpecialKey key -> (
      let y, x = Curses.getyx window in
      let maxy, maxx = Curses.getmaxyx window in
      match key with
      | Up when y - 1 >= 0 ->
          assert (Curses.move (y - 1) x);
          ()
      | (Enter | Down) when y + 1 < maxy ->
          assert (Curses.move (y + 1) x);
          ()
      | Left when x - 1 >= 0 ->
          assert (Curses.move y (x - 1));
          ()
      | Right when x + 1 <= maxx ->
          assert (Curses.move y (x + 1));
          ()
      | Escape ->
          editor.edit_mode <- Insert;
          assert (Curses.move (maxy - 1) 0);
          Curses.clrtoeol ();
          assert (Curses.addstr "--INSERT--");
          assert (Curses.move y x);
          ()
      | _ -> ())
  | NonSpecialKey _ -> ()
