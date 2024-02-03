(* Main module *)

open Oed
open Oed.Editor_state

let ( let* ) = EditorSt.bind

let prepare_state (mainw, subw) : Editor.t =
  let file, contents =
    if Array.length Sys.argv > 1 then
      (Some Sys.argv.(1), File.read_file Sys.argv.(1))
    else (None, "")
  in
  {
    fname = file;
    mode = Normal;
    buffer = TextBuffer.build contents;
    history = Gap_buffer.empty;
    mwin = mainw;
    swin = subw;
    off = 0;
  }

let rec main_handler :
    type a. (a -> bool EditorSt.t) -> a Key.keyset -> unit EditorSt.t =
 fun f x ->
  let* s = EditorSt.get in
  let key = Curses.wgetch s.mwin |> Key.convert x in
  let* r = EditorSt.catch (f key) (fun () -> EditorSt.fail) in
  if r then main_loop () else EditorSt.return ()

and main_loop () =
  let* s = EditorSt.get in
  match s.mode with
  | Normal -> main_handler Mode.normal_action Normal
  | Insert -> main_handler Mode.insert_action Insert

let init =
  let* () = Editor_io.refresh_screen in
  let* () = Editor_io.change_status "--NORMAL--" in
  main_loop ()

let () =
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
  let state : Editor.t = prepare_state (w_main, w_sub) in
  match EditorSt.run state init with
  | Some () ->
      Curses.endwin ();
      ()
  | None -> failwith "error ocurred"
