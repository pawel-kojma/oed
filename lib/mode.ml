open Key
open Editor_state

let ( let* ) = EditorSt.bind

let insert_action key =
  match key with
  | SpecialKey key -> (
      let* y, x = Editor_io.getyx in
      let* maxy, maxx = Editor_io.getmaxyx in
      match key with
      | Backspace ->
          let* () = Editor_io.backspace in
          EditorSt.return true
      | Enter ->
          let* () = Editor_io.enter in
          EditorSt.return true
      | Up when y - 1 >= 0 ->
          let* () = Editor_io.up in
          EditorSt.return true
      | Down when y + 1 < maxy ->
          let* () = Editor_io.down in
          EditorSt.return true
      | Left when x - 1 >= 0 ->
          let* () = Editor_io.left in
          EditorSt.return true
      | Right when x + 1 < maxx ->
          let* () = Editor_io.right in
          EditorSt.return true
      | Escape ->
          let* () = EditorSt.change Mode Normal in
          let* () = Editor_io.change_status "--NORMAL--" in
          EditorSt.return true
      | _ -> EditorSt.return true)
  | NonSpecialKey key ->
      let* () = Editor_io.inskey (char_of_int key) in
      EditorSt.return true

let normal_action key =
  match key with
  | SpecialKey key -> (
      let* y, x = Editor_io.getyx in
      let* maxy, maxx = Editor_io.getmaxyx in
      match key with
      | Up when y - 1 >= 0 ->
          let* () = Editor_io.up in
          EditorSt.return true
      | (Enter | Down) when y + 1 < maxy ->
          let* () = Editor_io.down in
          EditorSt.return true
      | Left when x - 1 >= 0 ->
          let* () = Editor_io.left in
          EditorSt.return true
      | Right when x + 1 < maxx ->
          let* () = Editor_io.right in
          EditorSt.return true
      | Escape ->
          let* () = EditorSt.change Mode Insert in
          let* () = Editor_io.change_status "--INSERT--" in
          EditorSt.return true
      | _ -> EditorSt.return true)
  | NonSpecialKey _ -> EditorSt.return true
