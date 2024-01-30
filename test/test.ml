open Oed
module T = Text_buffer.Make (Gap_buffer)

let unpack = function Either.Left n -> n | Either.Right n -> n
let rec right_n n t = if n > 0 then T.right t |> right_n (n - 1) else t
let rec left_n n t = if n > 0 then T.left t |> left_n (n - 1) else t
let%test "data_in_order" = T.build "ala ma kota" |> T.decompose = "ala ma kota"

let%test "data_in_order_nl" =
  T.build "ala ma kota\na kot ma ale"
  |> T.decompose = "ala ma kota\na kot ma ale"

let%expect_test "insert" =
  T.(build "abc" |> right |> insert 'd' |> decompose) |> print_endline;
  [%expect {| adbc |}]

let%expect_test "remove" =
  T.(build "abcd" |> right_n 2 |> remove |> decompose) |> print_endline;
  [%expect {| acd |}]

let%expect_test "down" =
  T.(build "linia\ndruga linia" |> right_n 2 |> down |> insert 'x' |> decompose)
  |> print_endline;
  [%expect {| 
  linia
  drxuga linia |}]

let%expect_test "up" =
  T.(
    build "linia\ndruga linia" |> right_n 2 |> down |> right_n 1 |> up
    |> insert 'x' |> decompose)
  |> print_endline;
  [%expect {|
  linxia
  druga linia |}]

let%expect_test "try_oob_right" =
  T.(build "abracadabra\nkamilslimak" |> right_n 999 |> remove |> decompose)
  |> print_endline;
  [%expect {|
    abracadabr
    kamilslimak |}]

let%expect_test "try_oob_left" =
  T.(
    build "abracadabra\nkamilslimak"
    |> down |> left_n 53 |> right |> remove |> decompose)
  |> print_endline;
  [%expect {|
    abracadabra
    amilslimak |}]

let%expect_test "long_to_short_down" =
  T.(build "abracadabra\nabc" |> right_n 6 |> down |> remove |> decompose)
  |> print_endline;
  [%expect {|
    abracadabra
    ab |}]

let%expect_test "long_to_short_up" =
  T.(
    build "abcd\nkamilslimak" |> down |> right_n 6 |> up |> remove |> insert 'x'
    |> decompose)
  |> print_endline;
  [%expect {|
    abcx
    kamilslimak |}]

let%expect_test "double_newline_down" =
  T.(build "\n\n" |> down |> insert 'x' |> decompose) |> print_endline;
  [%expect {|

    x
    |}]

let%expect_test "double_newline_up" =
  T.(build "\n\n" |> down |> up |> insert 'x' |> decompose) |> print_endline;
  [%expect {|
    x
    
    |}]
