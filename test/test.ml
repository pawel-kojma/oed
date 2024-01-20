open Oed
module T = Text_buffer.Make (Gap_buffer)

let unpack = function Either.Left n -> n | Either.Right n -> n
let%test "data_in_order" = T.build "ala ma kota" |> T.decompose = "ala ma kota"

let%test "it_all_comes_together" =
  T.(create_empty |> insert 'a' |> insert 'b' |> insert 'c' |> decompose)
  = "abc"

let%expect_test "substitute" =
  T.(create_empty |> insert 'a' |> insert 'x' |> set_at_cursor 'b' |> decompose)
  |> print_endline;
  [%expect {| ab |}]

let%expect_test _ =
  T.(build "abracadabra\n" |> next_nl_off |> unpack) |> print_int;
  [%expect {| 11 |}]

let%expect_test _ =
  T.(build "abracadabra\n" |> right_n 4 |> next_nl_off |> unpack) |> print_int;
  [%expect {| 7 |}]

let%expect_test _ =
  T.(build "abracadabra\nkamilslimak\n" |> right_n 11 |> next_nl_off |> unpack)
  |> print_int;
  [%expect {| 12 |}]

let%expect_test _ =
  T.(build "abracadabra\nkamilslimak\n" |> right_n 11 |> prev_nl_off |> unpack)
  |> print_int;
  [%expect {| 11 |}]

let%expect_test _ =
  T.(
    build "abc\nabracadabra\nkamilslimak\n"
    |> right_n 15 |> prev_nl_off |> unpack)
  |> print_int;
  [%expect {| 12 |}]

(* up arrow tests *)

let%expect_test _ =
  T.(
    build "abc\nabracadabra\nkamilslimak\n"
    |> right_n 15 |> up |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    abx
    abracadabra
    kamilslimak |}]

let%expect_test _ =
  T.(
    build "abc\nabracadabra\nkamilslimak\n"
    |> right_n 5 |> up |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    xbc
    abracadabra
    kamilslimak |}]

let%expect_test _ =
  T.(
    build "abc\nabracadabra\nkamilslimak\n"
    |> right_n 3 |> up |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    abx
    abracadabra
    kamilslimak |}]

let%expect_test _ =
  T.(
    build "abracadabra\nabc\nkamilslimak\n"
    |> right_n 14 |> up |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    axracadabra
    abc
    kamilslimak |}]

(* down arrow tests *)

let%expect_test _ =
  T.(
    build "abc\nabracadabra\nkamilslimak\n"
    |> right_n 15 |> down |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    abc
    abracadabra
    kamilslimax |}]

let%expect_test _ =
  T.(
    build "abc\nabracadabra\nkamilslimak\n"
    |> right_n 5 |> down |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    abc
    abracadabra
    xamilslimak |}]

let%expect_test _ =
  T.(build "a\nb\nc\n" |> right_n 2 |> down |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    a
    bxc |}]

let%expect_test _ =
  T.(build "a\nb\nc\n" |> right_n 4 |> down |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    a
    b
    cx |}]

let%expect_test _ =
  T.(
    build "abc\nabracadabra\nkamilslimak\n"
    |> right_n 20 |> down |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    abc
    abracadabra
    kamilslimakx |}]

let%expect_test _ =
  T.(
    build "abc\nabracadabra\nkamilslimak\n"
    |> down |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    abcxabracadabra
    kamilslimak |}]

let%expect_test _ =
  T.(
    build "abracadabra\nabc\nkamilslimak\n"
    |> right_n 1 |> down |> set_at_cursor 'x' |> decompose)
  |> print_string;
  [%expect {|
    abracadabra
    xbc
    kamilslimak |}]
