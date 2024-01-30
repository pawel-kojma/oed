open Oed
module T = Text_buffer.Make (Gap_buffer)

let unpack = function Either.Left n -> n | Either.Right n -> n
let%test "data_in_order" = T.build "ala ma kota" |> T.decompose = "ala ma kota"

let%test "data_in_order_nl" =
  T.build "ala ma kota\na kot ma ale"
  |> T.decompose = "ala ma kota\na kot ma ale"

let%test "insert" = T.(build "abc" |> right |> insert 'd' |> decompose) = "adbc"

let%test "remove" =
  T.(build "abcd" |> right |> right |> remove |> decompose) = "acd"
