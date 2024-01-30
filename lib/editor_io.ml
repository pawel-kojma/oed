open Editor_state
module TextBuffer = Text_buffer.Make (Gap_buffer)

let ( let* ) = EditorSt.bind
let curses_try b = if b then EditorSt.return () else EditorSt.fail

let getyx =
  let* s = EditorSt.get in
  Curses.getyx s.mwin |> EditorSt.return

let getmaxyx =
  let* s = EditorSt.get in
  Curses.getmaxyx s.mwin |> EditorSt.return

let mv y x =
  let* s = EditorSt.get in
  Curses.wmove s.mwin y x |> curses_try

let move_cursor f =
  let* s = EditorSt.get in
  let n, c = f s.buffer in
  let y, x = TextBuffer.of_cords c in
  let* () = EditorSt.change Buffer n in
  mv y x

let up = move_cursor TextBuffer.up
let down = move_cursor TextBuffer.down
let left = move_cursor TextBuffer.left
let right = move_cursor TextBuffer.right

let scroll_down =
  let* s = EditorSt.get in
  let line = TextBuffer.next_line s.buffer in
  match line with
  | None -> EditorSt.return ()
  | Some line ->
      let* y, x = getyx in
      let* () = Curses.wscrl s.mwin 1 |> curses_try in
      let* maxy, _ = getmaxyx in
      let* () = mv (maxy - 1) 0 in
      let* () = Curses.addstr line |> curses_try in
      mv y x

let inskey key =
  let* s = EditorSt.get in
  let n, _ = TextBuffer.insert key s.buffer in
  let* () = EditorSt.change Buffer n in
  let* () = Curses.insch (int_of_char key) |> curses_try in
  let* y, x = getyx in
  let* maxy, maxx = getmaxyx in
  if x + 1 >= maxx && y + 1 >= maxy then
    let* () = scroll_down in
    mv y 0
  else if x + 1 >= maxx then mv (y + 1) 0
  else mv y (x + 1)

let enter = inskey '\n'

let change_status str =
  let* s = EditorSt.get in
  let* () = Curses.wmove s.swin 0 0 |> curses_try in
  let () = Curses.clrtoeol () in
  let () = Curses.attron Curses.A.bold in
  let* () = Curses.addstr str |> curses_try in
  let () = Curses.attroff Curses.A.bold in
  EditorSt.return ()

let backspace = EditorSt.return ()
