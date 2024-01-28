open Key
open Editor_state

let ( let* ) = EditorSt.bind

let insert_action key =
  let* s = EditorSt.get in
  match key with
  | SpecialKey key -> (
      let* y, x = Editor_io.getyx in
      let* maxy, maxx = Editor_io.getmaxyx in
      match key with
      | Backspace -> Editor_IO.backspace
      | Enter -> Editor_IO.enter
      | Up when y - 1 >= 0 -> Editor_IO.up
      | Down when y + 1 < maxy -> Editor_IO.down
      | Left when x - 1 >= 0 -> Editor_IO.left
      | Right when x + 1 < maxx -> Editor_IO.right
      | Escape ->
          let* () = EditorSt.change Mode Normal in
          Editor_IO.change_status "--NORMAL--"
      | _ -> EditorSt.return true)
  | NonSpecialKey key -> Editor_IO.inskey key

let normal_action key =
  let* s = EditorSt.get in
  match key with
  | SpecialKey key -> (
      let* y, x = Editor_io.getyx in
      let* maxy, maxx = Editor_io.getmaxyx in
      match key with
      | Up when y - 1 >= 0 -> Editor_IO.up
      | (Enter | Down) when y + 1 < maxy -> Editor_IO.down
      | Left when x - 1 >= 0 -> Editor_IO.left
      | Right when x + 1 < maxx -> Editor_IO.right
      | Escape ->
          let* () = EditorSt.change Mode Insert in
          Editor_IO.change_status "--INSERT--"
      | _ -> EditorSt.return true)
  | NonSpecialKey _ -> EditorSt.return true
