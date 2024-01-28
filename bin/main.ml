(* Main module *)

open Oed
open Oed.Editor_state

let ( let* ) = EditorSt.bind

let rec main_loop () =
  let* st = EditorSt.get in
  let key = Curses.getch () |> Key.convert in
  match st.mode with
  | Normal ->
      let* r =
        EditorSt.catch (Mode.normal_action key) (fun () -> EditorSt.fail)
      in
      if r then main_loop () else EditorSt.return ()
  | Insert ->
      let* r =
        EditorSt.catch (Mode.insert_action key) (fun () -> EditorSt.fail)
      in
      if r then main_loop () else EditorSt.return ()

let () =
  Curses.use_env true;
  let _w = Curses.initscr () in
  let maxy, maxx = Curses.getmaxyx _w in
  let w_main = Curses.newwin (maxy - 2) maxx 0 0 in
  let w_sub = Curses.newwin 2 maxx (maxy - 2) 0 in
  assert (Curses.cbreak ());
  assert (Curses.noecho ());
  assert (Curses.intrflush w_main false);
  assert (Curses.intrflush w_sub false);
  assert (Curses.keypad w_main true);
  assert (Curses.keypad w_sub true);
  Curses.scrollok w_main true;
  let state : Editor.t =
    {
      mode = Normal;
      buffer = TextBuffer.create_empty;
      history = Gap_buffer.empty;
      mwin = w_main;
      swin = w_sub;
    }
  in
  match EditorSt.run state (main_loop ()) with
  | Some () -> ()
  | None -> failwith "error ocurred"
