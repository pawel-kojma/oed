open Oed
module T = Text_buffer.Make (Gap_buffer)

let unpack = function Either.Left n -> n | Either.Right n -> n
let rec right_n n t = if n > 0 then T.right t |> right_n (n - 1) else t
let rec left_n n t = if n > 0 then T.left t |> left_n (n - 1) else t
let%test "data_in_order" = T.build "ala ma kota" |> T.decompose = "ala ma kota"

let%test "data_in_order_nl" =
  T.build "ala ma kota\na kot ale\na\nb"
  |> T.decompose = "ala ma kota\na kot ale\na\nb"

let%test "data_in_order_nl" =
  T.build "ala ma kota\na kot ma ale"
  |> T.decompose = "ala ma kota\na kot ma ale"

let%expect_test "build_1" =
  let c = T.(build "linia\ndruga linia" |> right_n 2) in
  let _, i, s = T.debug_view c in
  let () = List.iter (Printf.printf "%d ") i in
  let () = print_string "|" in
  print_int s;
  [%expect {| 5 11 |2 |}]

let%expect_test "build_2" =
  let c = T.(build "linia\ndruga linia\ntrzecia\nasdf\n\n" |> right_n 4) in
  let _, i, s = T.debug_view c in
  let () = List.iter (Printf.printf "%d ") i in
  let () = print_string "|" in
  print_int s;
  [%expect {| 5 11 7 4 0 0 |4 |}]

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

let%expect_test "multi_line_travel" =
  T.(
    build "abracadabra\nkamilslimak123\ntrzecia linia"
    |> right_n 4 |> down |> down |> right_n 3 |> insert 'x' |> up |> insert 'y'
    |> decompose)
  |> print_endline;
  [%expect {|
  abracadabra
  kamilsliymak123
  trzeciax linia|}]

let%expect_test "multi_line_travel_same_length" =
  T.(
    build "abracadabra\nkamilslimak\nkalambury"
    |> right_n 4 |> down |> down |> right_n 3 |> insert 'x' |> insert 'y'
    |> right_n 2 |> up |> up |> insert 'X' |> decompose)
  |> print_endline;
  [%expect {|
  abracadabraX
  kamilslimak
  kalambuxyry|}]

let%expect_test "text_editing" =
  T.(
    build "a\nb\nc\n" |> right |> remove |> insert 'c' |> down |> remove
    |> insert 'a' |> down |> remove |> insert 'b' |> decompose)
  |> print_endline;
  [%expect {|
    c
    a
    b |}]

let%expect_test "integrity_after_moving" =
  let c =
    T.(
      build "ala ma\nkota\na\nkot ale"
      |> down |> down |> right_n 4 |> up |> left)
  in
  let _, i, s = T.debug_view c in
  let () = List.iter (Printf.printf "%d ") i in
  let () = print_string "|" in
  print_int s;
  [%expect {| 6 4 1 7 |0 |}]

let%expect_test "insert_newline" =
  T.(build "abracadabra" |> right_n 4 |> insert '\n' |> decompose)
  |> print_endline;
  [%expect {|
      abra
      cadabra |}]

let%expect_test "remove_newline" =
  T.(build "abra\ncadabra" |> down |> remove |> decompose) |> print_endline;
  [%expect {| abracadabra |}]

let%expect_test "next_line_1" =
  let s = T.(build "abracadabra\nkamilslimak" |> next_line) in
  match s with
  | None -> failwith "no string"
  | Some x ->
      print_endline x;
      [%expect {|kamilslimak|}]

let%expect_test "next_line_2" =
  let s =
    T.(build "abracadabra\nkamilslimak\nasdasdasd\nlinia z spacja" |> next_line)
  in
  match s with
  | None -> failwith "no string"
  | Some x ->
      print_endline x;
      [%expect {|kamilslimak|}]

let%expect_test "next_line_3" =
  let s = T.(build "abracadabra\nkamilslimak" |> right_n 4 |> next_line) in
  match s with
  | None -> failwith "no string"
  | Some x ->
      print_endline x;
      [%expect {|kamilslimak|}]

let%expect_test "next_line_4" =
  let s =
    T.(build "abracadabra\nkamilslimak\n" |> right_n 3 |> down |> next_line)
  in
  match s with
  | None -> failwith "no string"
  | Some x ->
      print_endline x;
      [%expect {||}]

let%expect_test "get_line_1" =
  let s =
    T.(build "ala ma\nkota\na\nkot ale" |> down |> right_n 2 |> get_line)
  in
  print_endline s;
  [%expect {| kota |}]

let%expect_test "get_line_2" =
  let s = T.(build "ala ma\nkota\na\nkot ale" |> down |> down |> get_line) in
  print_endline s;
  [%expect {| a |}]

let%expect_test "get_line_3" =
  let s =
    T.(build "ala ma\nkota\na\nkot ale" |> down |> down |> down |> get_line)
  in
  print_endline s;
  [%expect {| kot ale |}]

let%expect_test "get_line_4" =
  T.(build "ala ma\nkota\na\nkot ale" |> get_line) |> print_endline;
  [%expect {| ala ma |}]

let%expect_test "get_line_5" =
  T.(build "ala ma\nkota\na\n" |> down |> down |> down |> get_line)
  |> print_endline;
  [%expect {| |}]

let%expect_test "nth_next_line_1" =
  match T.(build "ala ma\nkota ,\na kot\nma\nale" |> nth_next_line 2) with
  | None -> print_endline "no string"
  | Some s ->
      print_endline s;
      [%expect {| a kot |}]

let%expect_test "nth_next_line_2" =
  match T.(build "ala ma\nkota a\n kot\nma\nale" |> nth_next_line 0) with
  | None -> print_endline "no string"
  | Some s ->
      print_endline s;
      [%expect {| ala ma |}]

let%expect_test "nth_next_line_3" =
  match T.(build "ala ma\nkota a\n kot\nma\nale" |> nth_next_line 4) with
  | None -> print_endline "no string"
  | Some s ->
      print_endline s;
      [%expect {| ale |}]

let%expect_test "nth_next_line_4" =
  match T.(build "ala ma\nkota a\n kot\nma\nale" |> nth_next_line 5) with
  | None ->
      print_endline "no string";
      [%expect {| no string |}]
  | Some s -> print_endline s

let%expect_test "cords_1" =
  let y, x = T.(build "abba\n" |> to_cords) in
  Printf.printf "%d %d" y x;
  [%expect {| 0 0 |}]

let%expect_test "cords_2" =
  let y, x = T.(build "abba\n" |> down |> to_cords) in
  Printf.printf "%d %d" x y;
  [%expect {| 0 1 |}]

let%expect_test "cords_3" =
  let y, x =
    T.(
      build "abba\nasdf\nacme" |> right |> down |> right_n 10 |> left
      |> to_cords)
  in
  Printf.printf "%d %d" x y;
  [%expect {| 3 1 |}]

let%expect_test "cords_4" =
  let y, x = T.(build "abracadabra" |> right_n 4 |> insert '\n' |> to_cords) in
  Printf.printf "%d %d" x y;
  [%expect {| 0 1 |}]

let%expect_test "cords_5" =
  let y, x = T.(build "abra\ncadabra" |> down |> remove |> to_cords) in
  Printf.printf "%d %d" x y;
  [%expect {| 4 0 |}]

let%expect_test "get_lines_1" =
  List.iter (Printf.printf "\"%s\"\n")
    T.(build "pierwsza\ndruga" |> get_lines 10);
  [%expect {|
    "pierwsza"
    "druga" |}]

let%expect_test "get_lines_2" =
  List.iter (Printf.printf "\"%s\"\n")
    T.(build "pierwsza\ndruga\n" |> get_lines 10);
  [%expect {|
    "pierwsza"
    "druga"
    "" |}]

let%expect_test "get_lines_3" =
  List.iter (Printf.printf "\"%s\"\n")
    T.(build "pierwsza\ndruga\ntrzecia" |> down |> get_lines 1);
  [%expect {| "druga" |}]

let%expect_test "get_lines_4" =
  List.iter (Printf.printf "\"%s\"\n")
    T.(build "pierwsza\ndruga\ntrzecia" |> down |> get_lines 0);
  [%expect {||}]
