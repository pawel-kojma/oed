open Editor_state
module History = Gap_buffer
module TextBuffer = Text_buffer.Make (Gap_buffer)

let ( let* ) = EditorSt.bind
let curses_try b = if b then EditorSt.return () else EditorSt.fail

let normalize_cords (y, x) =
  let* s = EditorSt.get in
  EditorSt.return (y - s.off, x)

let get_cords =
  let* s = EditorSt.get in
  TextBuffer.to_cords s.buffer |> normalize_cords

let getyx =
  let* s = EditorSt.get in
  Curses.getyx s.mwin |> EditorSt.return

let getmaxyx =
  let* s = EditorSt.get in
  Curses.getmaxyx s.mwin |> EditorSt.return

let mv y x =
  let* s = EditorSt.get in
  Curses.wmove s.mwin y x |> curses_try

let putstryx y x str =
  let* s = EditorSt.get in
  let* () = mv y x in
  let* () = Curses.waddstr s.mwin str |> curses_try in
  let* () = mv y x in
  Curses.wrefresh s.mwin |> curses_try

let scroll_down =
  let* s = EditorSt.get in
  let line = TextBuffer.get_line s.buffer in
  let* () = Curses.wscrl s.mwin 1 |> curses_try in
  let* maxy, _ = getmaxyx in
  let* () = mv (maxy - 1) 0 in
  let* () = Curses.waddstr s.mwin line |> curses_try in
  let* () = EditorSt.change Off (s.off + 1) in
  let* y, x = get_cords in
  mv y x

let scroll_up =
  let* s = EditorSt.get in
  let line = TextBuffer.get_line s.buffer in
  let* () = Curses.wscrl s.mwin (-1) |> curses_try in
  let* () = mv 0 0 in
  let* () = Curses.waddstr s.mwin line |> curses_try in
  let* () = EditorSt.change Off (s.off - 1) in
  let* y, x = get_cords in
  mv y x

let move_cursor f =
  let* s = EditorSt.get in
  let* () = f s.buffer |> EditorSt.change Buffer in
  let* y, x = get_cords in
  let* maxy, maxx = getmaxyx in
  if x < 0 || x >= maxx then
    failwith "line-wrap cursor movement not implemented"
  else if y < 0 then scroll_up
  else if y >= maxy then scroll_down
  else mv y x

let up = move_cursor TextBuffer.up
let down = move_cursor TextBuffer.down
let left = move_cursor TextBuffer.left
let right = move_cursor TextBuffer.right

let inskey key =
  let* s = EditorSt.get in
  let n = TextBuffer.insert key s.buffer in
  let* () = EditorSt.change Buffer n in
  let* _, x = get_cords in
  let* _, maxx = getmaxyx in
  if x >= maxx then failwith "line-wrap insert not implemented"
  else
    let* () = Curses.winsch s.mwin (int_of_char key) |> curses_try in
    let* y, x = get_cords in
    mv y x

let tab = List.init 4 (fun _ -> ' ') |> EditorSt.foldM (fun _ s -> inskey s) ()

let enter =
  let* s = EditorSt.get in
  let n = TextBuffer.insert '\n' s.buffer in
  let* () = EditorSt.change Buffer n in
  let line = TextBuffer.get_line n in
  let* y, x = get_cords in
  let* maxy, _ = getmaxyx in
  if y + 1 = maxy then
    let () = Curses.wclrtoeol s.mwin in
    let* () = mv y x in
    putstryx y x line
  else if y >= maxy then
    let () = Curses.wclrtoeol s.mwin in
    scroll_down
  else
    let () = Curses.wclrtoeol s.mwin in
    let* () = mv y x in
    let* () = Curses.winsertln s.mwin |> curses_try in
    putstryx y x line

let backspace =
  let* s = EditorSt.get in
  match TextBuffer.get_before_cursor s.buffer with
  | None -> EditorSt.return ()
  | Some ch when ch = '\n' -> (
      let line = TextBuffer.get_line s.buffer in
      let n = TextBuffer.remove s.buffer in
      let* () = EditorSt.change Buffer n in
      let* () = Curses.wdeleteln s.mwin |> curses_try in
      let* y, _ = get_cords in
      if y < 0 then
        let* () = scroll_up in
        let* y, x = get_cords in
        putstryx y x line
      else
        let* maxy, _ = getmaxyx in
        match TextBuffer.nth_next_line (maxy - y - 1) s.buffer with
        | None ->
            let* y, x = get_cords in
            putstryx y x line
        | Some l ->
            let* () = putstryx (maxy - 1) 0 l in
            let* y, x = get_cords in
            putstryx y x line)
  | Some _ ->
      let n = TextBuffer.remove s.buffer in
      let* () = EditorSt.change Buffer n in
      let* y, x = get_cords in
      let* () = Curses.mvwdelch s.mwin y x |> curses_try in
      mv y x

let delete =
  let* s = EditorSt.get in
  match TextBuffer.get_at_cursor s.buffer with
  | None -> EditorSt.return ()
  | Some _ ->
      let* () = EditorSt.change Buffer (TextBuffer.right s.buffer) in
      backspace

let log_subwindow str =
  let* s = EditorSt.get in
  let* () = Curses.wmove s.swin 1 0 |> curses_try in
  let () = Curses.wclrtoeol s.swin in
  let* () = Curses.waddstr s.swin str |> curses_try in
  let* () = Curses.wrefresh s.swin |> curses_try in
  let* y, x = get_cords in
  mv y x

let change_status str =
  let* s = EditorSt.get in
  let* () = Curses.wmove s.swin 0 0 |> curses_try in
  let () = Curses.wclrtoeol s.swin in
  let () = Curses.wattron s.swin Curses.A.bold in
  let* () = Curses.waddstr s.swin str |> curses_try in
  let () = Curses.wattroff s.swin Curses.A.bold in
  let* () = Curses.wrefresh s.swin |> curses_try in
  let* y, x = get_cords in
  mv y x

let maybe_insert_key key =
  try inskey (char_of_int key)
  with Invalid_argument _ ->
    log_subwindow ("No binding for key: " ^ Curses.keyname key)

let save_ctx =
  let* s = EditorSt.get in
  let* cords = get_cords in
  let ctx : Editor.ctx = { buffer = s.buffer; off = s.off; cords } in
  History.insert_before ctx s.history
  |> History.drop_after |> EditorSt.change History

let refresh_screen =
  let* s = EditorSt.get in
  let rec _up_n n c = if n <= 0 then c else _up_n (n - 1) (TextBuffer.up c) in
  let* y, _ = getyx in
  let* maxy, _ = getmaxyx in
  let lines = TextBuffer.get_lines maxy (_up_n y s.buffer) in
  let () = Curses.wclear s.mwin in
  let* () =
    EditorSt.iteriM
      (fun i line ->
        let* () = mv i 0 in
        Curses.waddstr s.mwin line |> curses_try)
      lines
  in
  Curses.wrefresh s.mwin |> curses_try

let restore_ctx (ctx : Editor.ctx) =
  let y, x = ctx.cords in
  let* () = EditorSt.change Buffer ctx.buffer in
  let* () = EditorSt.change Off ctx.off in
  let* () = mv y x in
  refresh_screen

let input_subwin =
  let rec _input_loop () =
    let* s = EditorSt.get in
    match Curses.wgetch s.mwin |> Key.convert Insert with
    | NonSpecialKeyI key ->
        let* () = maybe_insert_key key in
        _input_loop ()
    | SpecialKeyI key -> (
        match key with
        | Enter -> EditorSt.return (TextBuffer.decompose s.buffer)
        | Escape -> EditorSt.fail
        | Backspace ->
            let* () = backspace in
            _input_loop ()
        | _ -> _input_loop ())
  in
  let* s = EditorSt.get in
  let* _, maxx = getmaxyx in
  let* () = log_subwindow "File name:" in
  let dwin = Curses.derwin s.swin 1 (maxx - 12) 1 12 in
  let* () = Curses.intrflush dwin false |> curses_try in
  let* () = Curses.keypad dwin true |> curses_try in
  let state : Editor.t =
    {
      fname = None;
      mode = Insert;
      buffer = TextBuffer.build "";
      history = Gap_buffer.empty;
      mwin = dwin;
      swin = s.swin;
      off = 0;
      was_edited = false;
    }
  in
  let r = EditorSt.run state (_input_loop ()) in
  let* () = Curses.delwin dwin |> curses_try in
  EditorSt.return r

let save_buffer =
  let _save buf fname =
    TextBuffer.decompose buf |> File.write_file fname |> EditorSt.return
  in
  let* s = EditorSt.get in
  match s.fname with
  | None -> (
      let* fname = input_subwin in
      match fname with
      | None -> EditorSt.return false
      | Some fname ->
          let* () = EditorSt.change Fname (Some fname) in
          let* () = _save s.buffer fname in
          EditorSt.return true)
  | Some fname ->
      let* () = _save s.buffer fname in
      EditorSt.return true
