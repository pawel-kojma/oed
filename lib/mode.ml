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
      | Backspace -> Editor_io.backspace
      | Enter -> Editor_io.enter
      | Up when y - 1 >= 0 -> Editor_io.up
      | Down when y + 1 < maxy -> Editor_io.down
      | Left when x - 1 >= 0 -> Editor_io.left
      | Right when x + 1 < maxx -> Editor_io.right
      | Escape ->
          let* () = EditorSt.change Mode Normal in
          Editor_io.change_status "--NORMAL--"
      | _ -> EditorSt.return true)
  | NonSpecialKey key -> Editor_io.inskey key

let normal_action key =
  let* s = EditorSt.get in
  match key with
  | SpecialKey key -> (
      let* y, x = Editor_io.getyx in
      let* maxy, maxx = Editor_io.getmaxyx in
      match key with
      | Up when y - 1 >= 0 -> Editor_io.up
      | (Enter | Down) when y + 1 < maxy -> Editor_io.down
      | Left when x - 1 >= 0 -> Editor_io.left
      | Right when x + 1 < maxx -> Editor_io.right
      | Escape ->
          let* () = EditorSt.change Mode Insert in
          Editor_io.change_status "--INSERT--"
      | _ -> EditorSt.return true)
  | NonSpecialKey _ -> EditorSt.return true
