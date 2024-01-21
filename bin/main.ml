(* Main module *)

open Oed
open Oed.Editor_state

let ( let* ) = EditorSt.bind

let rec main_loop () =
  let* st = EditorSt.get in
  let key = Curses.getch () |> Key.convert in
  match st.mode with
  | Normal ->
      let* r = Mode.normal_action key in
      if r then main_loop () else EditorSt.return ()
  | Insert ->
      let* r = Mode.insert_action key in
      if r then main_loop () else EditorSt.return ()

let () =
  Curses.use_env true;
  let w = Curses.initscr () in
  assert (Curses.cbreak ());
  assert (Curses.noecho ());
  assert (Curses.intrflush w false);
  assert (Curses.keypad w true);
  assert (Curses.wmove w 0 0);
  Curses.scrollok w true;
  let state : Editor.t =
    {
      mode = Normal;
      buffer = TextBuffer.create_empty;
      history = Gap_buffer.empty;
      win = w;
    }
  in
  let r = EditorSt.run state (main_loop ()) in
  r

(*
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

*)
