module TextBuffer = Text_buffer.Make (Gap_buffer)

module Editor : sig
  type mode = Normal | Insert

  type 'a gadt =
    | Mode : mode gadt
    | Buffer : char Gap_buffer.t gadt
    | History : char Gap_buffer.t Gap_buffer.t gadt
    | Window : Curses.window gadt

  type t = {
    mode : mode;
    buffer : char Gap_buffer.t;
    history : char Gap_buffer.t Gap_buffer.t;
    win : Curses.window;
  }

  val change : 'a gadt -> 'a -> t -> t
end = struct
  type mode = Normal | Insert

  type t = {
    mode : mode;
    buffer : char Gap_buffer.t;
    history : char Gap_buffer.t Gap_buffer.t;
    win : Curses.window;
  }

  type 'a gadt =
    | Mode : mode gadt
    | Buffer : char Gap_buffer.t gadt
    | History : char Gap_buffer.t Gap_buffer.t gadt
    | Window : Curses.window gadt

  let change : type a. a gadt -> a -> t -> t =
   fun typ el s ->
    match typ with
    | Mode -> { mode = el; buffer = s.buffer; history = s.history; win = s.win }
    | Buffer -> { mode = s.mode; buffer = el; history = s.history; win = s.win }
    | History -> { mode = s.mode; buffer = s.buffer; history = el; win = s.win }
    | Window ->
        { mode = s.mode; buffer = s.buffer; history = s.history; win = el }
end

(* Monada stanu -- obliczenia z ukrytą komórką mutowalnego stanu *)
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
  val run : State.t -> 'a t -> 'a
  val change : 'a State.gadt -> 'a -> unit t
end = struct
  (* Obliczenie reprezentujemy jako funkcję z bieżącej wartości stanu w parę
   * wynik-nowy stan *)
  type 'a t = State.t -> 'a * State.t

  let return x s = (x, s)

  let bind m f s =
    let x, s = m s in
    f x s

  let get s = (s, s)
  let set s _ = ((), s)
  let run s m = fst (m s)

  let change : type a. a State.gadt -> a -> unit t =
   fun typ el s -> set (State.change typ el s) s
end

module EditorSt = St (Editor)
