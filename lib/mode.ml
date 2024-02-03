open Key
open Editor_state
module History = Gap_buffer

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
  let* s = EditorSt.get in
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
      | Save ->
          let* () = Editor_io.save_ctx in
          let* () = Editor_io.save_buffer in
          EditorSt.return true
      | Undo -> (
          match History.elem_before s.history with
          | None -> EditorSt.return true
          | Some ctx ->
              let* () = Editor_io.restore_ctx ctx in
              let* () = EditorSt.change History (History.move_left s.history) in
              EditorSt.return true)
      | Redo -> (
          match History.elem_at s.history with
          | None -> EditorSt.return true
          | Some ctx ->
              let* () = Editor_io.restore_ctx ctx in
              let* () =
                EditorSt.change History (History.move_right s.history)
              in
              EditorSt.return true)
      | _ -> EditorSt.return true)
  | NonSpecialKeyN _ -> EditorSt.return true
