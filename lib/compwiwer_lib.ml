open! Core
open Async

let test s = print_endline s

let command =
  Command.async ~summary:"entrypoint"
    ~readme:(fun () -> "asdf")
    (let%map_open.Command path = anon ("source" %: string) in
     fun () ->
       test path;
       return ())

let%expect_test "asdf" =
  test "nyaa";
  [%expect {| nyaa |}];
  return ()
