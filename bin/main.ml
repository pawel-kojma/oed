(* Main module *)

open Oed

let rec main_loop (ed : Mode.editor) (w : Curses.window) =
  let key = Curses.getch () |> Key.convert in
  match ed.edit_mode with
  | Normal ->
      ed.normal_action ed w key;
      main_loop ed w;
      ()
  | Insert ->
      ed.insert_action ed w key;
      main_loop ed w;
      ()

let () =
  Curses.use_env true;
  let w = Curses.initscr () in
  assert (Curses.cbreak ());
  assert (Curses.noecho ());
  assert (Curses.intrflush w false);
  assert (Curses.keypad w true);
  assert (Curses.wmove w 0 0);
  Curses.scrollok w true;
  let ed : Mode.editor =
    {
      edit_mode = Mode.Insert;
      insert_action = Mode.insert_mode_action;
      normal_action = Mode.normal_mode_action;
    }
  in
  main_loop ed w;
  Curses.endwin ();
  ()
