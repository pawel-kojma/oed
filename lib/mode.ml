open Key
open Editor_state

let ( let* ) = EditorSt.bind

let insert_action key =
  let* s = EditorSt.get in
  match key with
  | SpecialKey key -> (
      let y, x = Curses.getyx s.win in
      let maxy, maxx = Curses.getmaxyx s.win in
      match key with
      | Backspace ->
          assert (Curses.mvwdelch s.win y (x - 1));
          EditorSt.return true
      | Enter when y + 1 >= maxy ->
          assert (Curses.scroll s.win);
          assert (Curses.move y 0);
          EditorSt.return true
      | Enter when y + 1 < maxy ->
          assert (Curses.addstr "\n");
          EditorSt.return true
      | Up when y - 1 >= 0 ->
          assert (Curses.move (y - 1) x);
          EditorSt.return true
      | Down when y + 1 < maxy ->
          assert (Curses.move (y + 1) x);
          EditorSt.return true
      | Left when x - 1 >= 0 ->
          assert (Curses.move y (x - 1));
          EditorSt.return true
      | Right when x + 1 < maxx ->
          assert (Curses.move y (x + 1));
          EditorSt.return true
      | Escape ->
          let* () = EditorSt.change Mode Normal in
          assert (Curses.move (maxy - 1) 0);
          Curses.clrtoeol ();
          assert (Curses.addstr "--NORMAL--");
          assert (Curses.move y x);
          EditorSt.return true
      | _ -> EditorSt.return true)
  | NonSpecialKey key ->
      assert (Curses.addch key);
      EditorSt.return true

let normal_action key =
  let* s = EditorSt.get in
  match key with
  | SpecialKey key -> (
      let y, x = Curses.getyx s.win in
      let maxy, maxx = Curses.getmaxyx s.win in
      match key with
      | Up when y - 1 >= 0 ->
          assert (Curses.move (y - 1) x);
          EditorSt.return true
      | (Enter | Down) when y + 1 < maxy ->
          assert (Curses.move (y + 1) x);
          EditorSt.return true
      | Left when x - 1 >= 0 ->
          assert (Curses.move y (x - 1));
          EditorSt.return true
      | Right when x + 1 <= maxx ->
          assert (Curses.move y (x + 1));
          EditorSt.return true
      | Escape ->
          let* () = EditorSt.change Mode Insert in
          assert (Curses.move (maxy - 1) 0);
          Curses.clrtoeol ();
          assert (Curses.addstr "--INSERT--");
          assert (Curses.move y x);
          EditorSt.return true
      | _ -> EditorSt.return true)
  | NonSpecialKey _ -> EditorSt.return true
