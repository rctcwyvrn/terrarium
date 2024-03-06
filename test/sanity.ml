open! Core

let%expect_test "asdf" =
  print_endline "nyaaa";
  [%expect {| nyaaa |}]
;;
