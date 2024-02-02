open Key
open Editor_state

let ( let* ) = EditorSt.bind

let insert_action key =
  match key with
  | SpecialKeyI key -> (
      match key with
      | Backspace ->
          let* () = Editor_io.backspace in
          EditorSt.return true
      | Enter ->
          let* () = Editor_io.enter in
          EditorSt.return true
      | Up ->
          let* () = Editor_io.up in
          EditorSt.return true
      | Down ->
          let* () = Editor_io.down in
          EditorSt.return true
      | Left ->
          let* () = Editor_io.left in
          EditorSt.return true
      | Right ->
          let* () = Editor_io.right in
          EditorSt.return true
      | Escape ->
          let* () = EditorSt.change Mode Normal in
          let* () = Editor_io.change_status "--NORMAL--" in
          EditorSt.return true)
  | NonSpecialKeyI key ->
      let* () = Editor_io.inskey (char_of_int key) in
      EditorSt.return true

let normal_action key =
  match key with
  | SpecialKeyN key -> (
      match key with
      | Up ->
          let* () = Editor_io.up in
          EditorSt.return true
      | Enter | Down ->
          let* () = Editor_io.down in
          EditorSt.return true
      | Left ->
          let* () = Editor_io.left in
          EditorSt.return true
      | Right ->
          let* () = Editor_io.right in
          EditorSt.return true
      | Escape ->
          let* () = EditorSt.change Mode Insert in
          let* () = Editor_io.change_status "--INSERT--" in
          EditorSt.return true
      | Quit -> EditorSt.return false
      | _ -> EditorSt.return true)
  | NonSpecialKeyN _ -> EditorSt.return true
