module TextBuffer = Text_buffer.Make (Gap_buffer)

module Editor : sig
  type mode = Normal | Insert
  type ctx = { buffer : TextBuffer.t; off : int; cords : int * int }

  type 'a gadt =
    | Fname : string option gadt
    | Mode : mode gadt
    | Buffer : TextBuffer.t gadt
    | History : ctx Gap_buffer.t gadt
    | MainWindow : Curses.window gadt
    | SubWindow : Curses.window gadt
    | Off : int gadt
    | Was_edited : bool gadt

  type t = {
    fname : string option;
    mode : mode;
    buffer : TextBuffer.t;
    history : ctx Gap_buffer.t;
    mwin : Curses.window;
    swin : Curses.window;
    off : int;
    was_edited : bool;
  }

  val change : 'a gadt -> 'a -> t -> t
end = struct
  type mode = Normal | Insert
  type ctx = { buffer : TextBuffer.t; off : int; cords : int * int }

  type 'a gadt =
    | Fname : string option gadt
    | Mode : mode gadt
    | Buffer : TextBuffer.t gadt
    | History : ctx Gap_buffer.t gadt
    | MainWindow : Curses.window gadt
    | SubWindow : Curses.window gadt
    | Off : int gadt
    | Was_edited : bool gadt

  type t = {
    fname : string option;
    mode : mode;
    buffer : TextBuffer.t;
    history : ctx Gap_buffer.t;
    mwin : Curses.window;
    swin : Curses.window;
    off : int;
    was_edited : bool;
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
          off = s.off;
          fname = s.fname;
          was_edited = s.was_edited;
        }
    | Buffer ->
        {
          mode = s.mode;
          buffer = el;
          history = s.history;
          mwin = s.mwin;
          swin = s.swin;
          off = s.off;
          fname = s.fname;
          was_edited = s.was_edited;
        }
    | History ->
        {
          mode = s.mode;
          buffer = s.buffer;
          history = el;
          mwin = s.mwin;
          swin = s.swin;
          off = s.off;
          fname = s.fname;
          was_edited = s.was_edited;
        }
    | MainWindow ->
        {
          mode = s.mode;
          buffer = s.buffer;
          history = s.history;
          mwin = el;
          swin = s.swin;
          off = s.off;
          fname = s.fname;
          was_edited = s.was_edited;
        }
    | SubWindow ->
        {
          mode = s.mode;
          buffer = s.buffer;
          history = s.history;
          mwin = s.mwin;
          swin = el;
          off = s.off;
          fname = s.fname;
          was_edited = s.was_edited;
        }
    | Off ->
        {
          mode = s.mode;
          buffer = s.buffer;
          history = s.history;
          mwin = s.mwin;
          swin = s.swin;
          off = el;
          fname = s.fname;
          was_edited = s.was_edited;
        }
    | Fname ->
        {
          mode = s.mode;
          buffer = s.buffer;
          history = s.history;
          mwin = s.mwin;
          swin = s.swin;
          off = s.off;
          fname = el;
          was_edited = s.was_edited;
        }
    | Was_edited ->
        {
          mode = s.mode;
          buffer = s.buffer;
          history = s.history;
          mwin = s.mwin;
          swin = s.swin;
          off = s.off;
          fname = s.fname;
          was_edited = el;
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

  (* iterate over list, applying enumerated function,
     each with same state, discarding result *)
  val iteriM : (int -> 'a -> unit t) -> 'a list -> unit t

  (* like normal fold, but with monad expressions *)
  val foldM : ('a -> 'b -> 'a t) -> 'a -> 'b list -> 'a t
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
  let ( let* ) = bind

  let iteriM f lst s =
    return
      (List.iteri
         (fun i el -> match f i el s with None -> () | Some (x, _) -> x)
         lst)
      s

  let rec foldM f acc lst =
    match lst with
    | [] -> return acc
    | hd :: tl ->
        let* a = f acc hd in
        foldM f a tl
end

module EditorSt = St (Editor)
