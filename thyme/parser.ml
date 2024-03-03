open! Core
open! Soil
(*
   type 'a t = {
     parse: string -> ('a With_report.t
     ; info : Fragment.t list }

     let init ~parse = {parse ; info = []}

   let any = init (fun s -> match s with
   | c::cs -> (cs)) *)

module M = struct
  type 'a t = { parse : char list -> char list * 'a Or_error.t }

  let run { parse } s = parse s

  let bind { parse } ~f =
    {
      parse =
        (fun string ->
          match parse string with
          | rest, Ok v -> run (f v) rest
          | rest, Error e -> (rest, Error e));
    }

  let return v = { parse = (fun s -> (s, Ok v)) }
  let parse_error e = { parse = (fun s -> (s, e)) }
  let map = `Define_using_bind
end

include M
include Monad.Make (M)

let any =
  {
    parse =
      (fun string ->
        match string with
        | first :: rest -> (rest, Ok first)
        | [] -> ([], error_s [%message "unexpected end of input"]));
  }

let eof =
  {
    parse =
      (fun string ->
        match string with
        | _ :: _ -> (string, error_s [%message "expected EOF"])
        | [] -> ([], Ok ()));
  }

let exact_char expected =
  let open Let_syntax in
  let%bind matched = any in
  if Char.equal expected matched then return matched
  else
    parse_error
      (error_s [%message "unexpected char" (expected : char) (matched : char)])

let sequential operations =
  let open Let_syntax in
  let%map reversed =
    List.fold operations ~init:(return []) ~f:(fun results t ->
        let%bind results = results in
        let%bind next = t in
        return (next :: results))
  in
  List.rev reversed

let exact_string expected =
  let open Let_syntax in
  let%map chars =
    String.to_list expected |> List.map ~f:exact_char |> sequential
  in
  String.of_char_list chars

(* Runs the given parser, but only consumes input on success *)
let peeking parser =
  {
    parse =
      (fun string ->
        match run parser string with
        | _, Error e -> (string, Error e)
        | success -> success);
  }

(* Try to parse, only consuming input on success, returns a Result *)
let try_ parser =
  {
    parse =
      (fun string ->
        match run (peeking parser) string with
        | rest, Ok v -> (rest, Ok (Ok v))
        | rest, Error e -> (rest, Ok (Error e)));
  }

(* Try the parsers from left to right, parsing the first one that consumes input

   If all parsers don't match, returns an error (without consuming input) *)
let choice parsers =
  let left_then_right left right =
    {
      parse =
        (fun initial ->
          match run left initial with
          | rest, Error _ when List.equal Char.equal rest initial ->
              run right initial
          | rest, left_result -> (rest, left_result));
    }
  in
  List.map parsers ~f:peeking
  |> List.fold_right ~f:left_then_right
       ~init:(parse_error (error_s [%message "choice: all parsers failed"]))

let rec one_or_more parser =
  let open Let_syntax in
  let%bind first = parser in
  let%map rest = try_ (one_or_more parser) in
  match rest with
  | Ok matched_more -> first :: matched_more
  | Error _ -> first :: []

let zero_or_more parser = choice [ one_or_more parser; return [] ]

let rec repeat parser ~until =
  let open Let_syntax in
  match%bind try_ parser with
  | Error _ ->
      (* done *)
      return []
  | Ok v -> (
      match%bind try_ until with
      | Error _ ->
          (* doesn't match the stopper, so continue *)
          let%bind rest = repeat parser ~until in
          return (v :: rest)
      | Ok _ ->
          (* Stop *)
          return [ v ])

let match_if parser ~pred =
  let open Let_syntax in
  peeking
    (let%bind v = parser in
     if pred v then return v
     else parse_error (error_s [%message "erased by the parent peeking"]))

let whitespace_char = match_if any ~pred:Char.is_whitespace

let any_word =
  let open Let_syntax in
  let%map chars = repeat any ~until:whitespace_char in
  String.of_char_list chars

let parse_complete parser s =
  match run parser (String.to_list s) with
  | [], result -> result
  | remaining, result ->
      error_s
        [%message
          "did not finish parsing input"
            (remaining : char list)
            (result : _ Or_error.t)]

let%expect_test "any" =
  run any (String.to_list "asdf")
  |> [%sexp_of: char list * char Or_error.t] |> print_s;
  [%expect {|
    ((s d f) (Ok a)) |}]

let%expect_test "eof" =
  run eof (String.to_list "asdf")
  |> [%sexp_of: char list * unit Or_error.t] |> print_s;
  [%expect {|
  ((a s d f) (Error "expected EOF")) |}]

let%expect_test "eof" =
  run eof [] |> [%sexp_of: char list * unit Or_error.t] |> print_s;
  [%expect {|
  (() (Ok ())) |}]

let%expect_test "combined" =
  let open Let_syntax in
  let parser =
    let%bind first = any in
    let%bind second = any in
    let%bind third = any in
    let%bind () = eof in
    return (first, second, third)
  in
  parse_complete parser "abc"
  |> [%sexp_of: (char * char * char) Or_error.t] |> print_s;
  [%expect {| (Ok (a b c)) |}]

let%expect_test "string" =
  let open Let_syntax in
  let parser =
    let%bind parsed = exact_string "nyaa" in
    let%map () = eof in
    [%message "success" (parsed : string)]
  in
  parse_complete parser "nyaa" |> [%sexp_of: Sexp.t Or_error.t] |> print_s;
  [%expect {| (Ok (success (parsed nyaa))) |}]

let%expect_test "choice: fail" =
  let parser = choice [ exact_string "not"; exact_string "right" ] in
  parse_complete parser "different" |> [%sexp_of: string Or_error.t] |> print_s;
  [%expect
    {|
      (Error
       ("did not finish parsing input" (remaining (d i f f e r e n t))
        (result (Error "choice: all parsers failed")))) |}]

let%expect_test "choice: fail" =
  let parser =
    choice [ exact_string "not"; exact_string "right"; exact_string "match" ]
  in
  parse_complete parser "match" |> [%sexp_of: string Or_error.t] |> print_s;
  [%expect {|
      (Ok match) |}]

let%expect_test "repetitions: one or more (success)" =
  let parser = one_or_more any in
  parse_complete parser "asdf" |> [%sexp_of: char list Or_error.t] |> print_s;
  [%expect {|
    (Ok (a s d f)) |}]

let%expect_test "repetitions: one or more (fail)" =
  let parser = one_or_more any in
  parse_complete parser "" |> [%sexp_of: char list Or_error.t] |> print_s;
  [%expect {|
    (Error "unexpected end of input") |}]

let%expect_test "repetitions: zero or more (success)" =
  let parser = zero_or_more any in
  parse_complete parser "asdf" |> [%sexp_of: char list Or_error.t] |> print_s;
  [%expect {|
    (Ok (a s d f)) |}]

let%expect_test "repetitions: zero or more (empty)" =
  let parser = zero_or_more any in
  parse_complete parser "" |> [%sexp_of: char list Or_error.t] |> print_s;
  [%expect {|
    (Ok ()) |}]

let%expect_test "word" =
  let parser =
    let open Let_syntax in
    let%bind first = any_word in
    let%bind second = any_word in
    return [%message (first : string) (second : string)]
  in
  parse_complete parser "first_word second_word"
  |> [%sexp_of: Sexp.t Or_error.t] |> print_s;
  [%expect {| (Ok ((first first_word) (second second_word))) |}]
