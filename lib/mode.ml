open Key
open Editor_state
module History = Gap_buffer

let ( let* ) = EditorSt.bind

let insert_action key =
  let* s = EditorSt.get in
  match key with
  | SpecialKeyI key -> (
      match key with
      | Backspace ->
          let* () = Editor_io.backspace in
          let* () = EditorSt.change Was_edited true in
          EditorSt.return true
      | Enter ->
          let* () = Editor_io.enter in
          let* () = EditorSt.change Was_edited true in
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
          if s.was_edited then
            let* () = Editor_io.save_ctx in
            let* () = EditorSt.change Was_edited false in
            EditorSt.return true
          else EditorSt.return true)
  | NonSpecialKeyI key ->
      let* () = Editor_io.inskey (char_of_int key) in
      let* () = EditorSt.change Was_edited true in
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
          let* () = Editor_io.save_buffer in
          EditorSt.return true
      | Undo -> (
          match History.elem_before s.history with
          | None ->
              let* () = Editor_io.log_subwindow "Already at oldest change" in
              EditorSt.return true
          | Some ctx ->
              let* () = Editor_io.restore_ctx ctx in
              let* () = EditorSt.change History (History.move_left s.history) in
              let* () =
                Printf.sprintf "%d# after" (History.elements_after s.history)
                |> Editor_io.log_subwindow
              in

              EditorSt.return true)
      | Redo -> (
          match History.elem_at s.history with
          | None ->
              let* () = Editor_io.log_subwindow "Already at newest change" in
              EditorSt.return true
          | Some ctx ->
              let* () = Editor_io.restore_ctx ctx in
              let* () =
                EditorSt.change History (History.move_right s.history)
              in
              let* () =
                Printf.sprintf "%d# before" (History.elements_after s.history)
                |> Editor_io.log_subwindow
              in
              EditorSt.return true)
      | _ -> EditorSt.return true)
  | NonSpecialKeyN _ -> EditorSt.return true
