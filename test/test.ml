open Oed
module T = Text_buffer.Make (Gap_buffer)

let%test "data_in_order" = T.build "ala ma kota" |> T.decompose = "ala ma kota"

let%test "it_all_comes_together" =
  T.(create_empty |> insert 'a' |> insert 'b' |> insert 'c' |> decompose)
  = "abc"

(* let%test "empty_is_empty" = T.is_empty T.empty *)
