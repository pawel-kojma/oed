module TextBuffer = Text_buffer.Make (Gap_buffer)

module Editor : sig
  type mode = Normal | Insert

  type 'a gadt =
    | Mode : mode gadt
    | Buffer : TextBuffer.t gadt
    | History : TextBuffer.t Gap_buffer.t gadt
    | MainWindow : Curses.window gadt
    | SubWindow : Curses.window gadt

  type t = {
    mode : mode;
    buffer : TextBuffer.t;
    history : TextBuffer.t Gap_buffer.t;
    mwin : Curses.window;
    swin : Curses.window;
  }

  val change : 'a gadt -> 'a -> t -> t
end = struct
  type mode = Normal | Insert

  type 'a gadt =
    | Mode : mode gadt
    | Buffer : TextBuffer.t gadt
    | History :  TextBuffer.t Gap_buffer.t gadt
    | MainWindow : Curses.window gadt
    | SubWindow : Curses.window gadt

  type t = {
    mode : mode;
    buffer : TextBuffer.t;
    history : TextBuffer.t Gap_buffer.t;
    mwin : Curses.window;
    swin : Curses.window;
  }

  let change : type a. a gadt -> a -> t -> t =
   fun typ el s ->
    match typ with
    | Mode ->
        {
          mode = el;
          buffer = s.buffer;
          history = s.history;
          mwin = s.mwin;
          swin = s.swin;
        }
    | Buffer ->
        {
          mode = s.mode;
          buffer = el;
          history = s.history;
          mwin = s.mwin;
          swin = s.swin;
        }
    | History ->
        {
          mode = s.mode;
          buffer = s.buffer;
          history = el;
          mwin = s.mwin;
          swin = s.swin;
        }
    | MainWindow ->
        {
          mode = s.mode;
          buffer = s.buffer;
          history = s.history;
          mwin = el;
          swin = s.swin;
        }
    | SubWindow ->
        {
          mode = s.mode;
          buffer = s.buffer;
          history = s.history;
          mwin = s.mwin;
          swin = el;
        }
end

(* Monada stanu i błędu -- obliczenia z ukrytą komórką mutowalnego stanu, które mogą się udać albo nie udać *)
module St (State : sig
  type t
  type 'a gadt

  val change : 'a gadt -> 'a -> t -> t
end) : sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (* Pobierz stan *)
  val get : State.t t

  (* Ustaw stan *)
  val set : State.t -> unit t
  val run : State.t -> 'a t -> 'a option
  val change : 'a State.gadt -> 'a -> unit t

  (* Błąd *)
  val fail : 'a t

  (* try-catch *)
  val catch : 'a t -> (unit -> 'a t) -> 'a t
end = struct
  (* Obliczenie reprezentujemy jako funkcję z bieżącej wartości stanu w opcjonalną parę
   * wynik-nowy stan *)
  type 'a t = State.t -> ('a * State.t) option

  let return x s = Some (x, s)
  let bind m f s = match m s with Some (x, s) -> f x s | None -> None
  let get s = Some (s, s)
  let set s _ = Some ((), s)
  let run s m = match m s with Some (x, _) -> Some x | None -> None

  let change : type a. a State.gadt -> a -> unit t =
   fun typ el s -> set (State.change typ el s) s

  let fail _ = None
  let catch m f s = match m s with Some (x, s) -> Some (x, s) | None -> f () s
end

module EditorSt = St (Editor)
