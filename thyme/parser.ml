open! Core
open! Soil
(*
   type 'a t = {
     parse: string -> ('a With_report.t
     ; info : Fragment.t list }

     let init ~parse = {parse ; info = []}

   let any = init (fun s -> match s with
   | c::cs -> (cs)) *)

(* - move source code positions, pass in a string wrapper with accessors by line+col?
   - accumulate debug fragments
   - make progress only possible on non-error? should everything just be peeking? just needs a change in
   the two base cases and [bind]
*)
module M = struct
  module Out = struct
    (**  A [Parser.t] when given a string outputs either 
        - A match, with the matched value, rest of the output, and debug logs
        - No match, with debug + error logs *)

    type 'a t =
      | Match of { rest : char list; v : 'a; info : Fragment.t list }
      | No_match of Fragment.t list
    [@@deriving sexp_of]

    let tag t ~tag =
      match t with
      | Match t ->
          Match { t with info = List.map ~f:(Fragment.tag ~tag) t.info }
      | No_match info -> No_match (List.map ~f:(Fragment.tag ~tag) info)
  end

  type 'a t = F of (char list -> 'a Out.t)

  let run (F parse) s = parse s

  let bind (F parse) ~f =
    F
      (fun string ->
        match parse string with
        | Match { rest; v; info } -> (
            match run (f v) rest with
            | Match { rest; v; info = new_info } ->
                Match { rest; v; info = info @ new_info }
            | No_match new_info -> No_match (info @ new_info))
        | No_match info -> No_match info)

  let return' ?here v =
    let info =
      match here with
      | Some loc -> [ Fragment.init Debug [%message "thyme->atom"] loc ]
      | None -> []
    in
    F (fun s -> Match { rest = s; v; info })

  let return v = return' v

  let parse_error error ~here =
    F
      (fun _ ->
        No_match
          [
            Fragment.init Error
              [%message "thyme->parse_failed" (error : Sexp.t)]
              here;
          ])

  let tag (F parse) ~tag = F (Fn.compose (Out.tag ~tag) parse)

  let add_info t ~frag =
    bind t ~f:(fun v -> F (fun s -> Match { rest = s; v; info = [ frag ] }))

  let map = `Define_using_bind
end

include M
include Monad.Make (M)

let any =
  F
    (fun string ->
      match string with
      | first :: rest -> Match { rest; v = first; info = [] }
      | [] -> No_match [])

let eof ~here =
  F
    (fun string ->
      match string with
      | _first :: _rest ->
          No_match [ Fragment.init Error [%message "expected eof"] here ]
      | [] -> Match { rest = []; v = (); info = [] })

let sequential operations =
  let open Let_syntax in
  let%map reversed =
    List.fold operations ~init:(return []) ~f:(fun results t ->
        let%bind results = results in
        let%bind next = t in
        return (next :: results))
  in
  List.rev reversed

(* Peek ahead and see if the parser matches

   Never consumes *)
let lookahead_matches parser =
  F
    (fun string ->
      match run parser string with
      | Match { info; _ } ->
          let info =
            List.map info
              ~f:
                (Fragment.tag
                   ~tag:
                     (Fragment.init Info [%message "thyme->lookahead"] [%here]))
          in
          Match { rest = string; v = true; info }
      (* Ignore the extra info on no_match? *)
      | No_match _info -> Match { rest = string; v = false; info = [] })

(* Try the parsers from left to right, parsing the first one that consumes input

   If all parsers don't match, returns an error (without consuming input) *)
let rec choice parsers_to_try ~here =
  let open Let_syntax in
  match parsers_to_try with
  | [] -> parse_error [%message "thyme->choice: No choices match"] ~here
  | parser :: rest ->
      if%bind lookahead_matches parser then parser else choice rest ~here

let try_ parser =
  F
    (fun string ->
      match run parser string with
      | Match { rest; v; info } -> Match { rest; v = Some v; info }
      (* Drop all state from stepping forward *)
      | No_match _info -> Match { rest = string; v = None; info = [] })

let one_or_more parser ~here =
  let rec again () =
    let open Let_syntax in
    let%bind first = parser in
    let%map rest = try_ (again ()) in
    match rest with
    | Some matched_more -> first :: matched_more
    | None -> [ first ]
  in
  tag (again ()) ~tag:(Fragment.init Info [%message "thyme->one_or_more"] here)

let zero_or_more parser ~here =
  choice [ one_or_more parser ~here; return [] ] ~here

let rec repeat parser ~until =
  let open Let_syntax in
  if%bind lookahead_matches until then (* stop repeating *)
    return []
  else
    match%bind try_ parser with
    | None ->
        (* done *)
        return []
    | Some v ->
        let%bind rest = repeat parser ~until in
        return (v :: rest)

let match_and_assert parser ~here ~pred ~pred_msg =
  let open Let_syntax in
  let%bind v = parser in
  if pred v then return v
  else
    parse_error
      [%message "thyme->match_and_assert: Assertion failed" (pred_msg : Sexp.t)]
      ~here

let whitespace_char =
  match_and_assert any ~pred:Char.is_whitespace ~here:[%here]
    ~pred_msg:[%message "Char.is_whitespace"]

let exact_char expected =
  match_and_assert any ~pred:(Char.equal expected) ~here:[%here]
    ~pred_msg:[%message "Char.equal" (expected : char)]

let exact_string expected ~here =
  String.to_list expected |> List.map ~f:exact_char |> sequential
  >>| String.of_char_list
  |> add_info
       ~frag:
         (Fragment.init Debug
            [%message "thyme->exact_string" (expected : string)]
            here)

let any_word ~here =
  (let open Let_syntax in
   let%map chars = repeat any ~until:whitespace_char in
   String.of_char_list chars)
  |> add_info ~frag:(Fragment.init Debug [%message "thyme->any_word"] here)

let parse_complete parser s =
  match run parser (String.to_list s) with
  | Match { rest = []; v; info } -> Ok (v, info)
  | Match { rest = remaining; v = _; info } ->
      error_s
        [%message
          "did not finish parsing input"
            (remaining : char list)
            (info : Fragment.t list)]
  | No_match info -> error_s [%message "parser failed" (info : Fragment.t list)]

let%expect_test "any" =
  run any (String.to_list "asdf") |> [%sexp_of: char Out.t] |> print_s;
  [%expect {|
    (Match (rest (s d f)) (v a) (info ())) |}]

let%expect_test "eof" =
  run (eof ~here:[%here]) (String.to_list "asdf")
  |> [%sexp_of: unit Out.t] |> print_s;
  [%expect
    {|
  (No_match
   (((label ()) (kind Error) (message "expected eof") (here <opaque>)
     (refers_to ()) (tags ())))) |}]

let%expect_test "eof" =
  run (eof ~here:[%here]) [] |> [%sexp_of: unit Out.t] |> print_s;
  [%expect {|
  (Match (rest ()) (v ()) (info ())) |}]

let%expect_test "combined" =
  let open Let_syntax in
  let parser =
    let%bind first = any in
    let%bind second = any in
    let%bind third = any in
    let%bind () = eof ~here:[%here] in
    return (first, second, third)
  in
  parse_complete parser "abc"
  |> [%sexp_of: ((char * char * char) * Fragment.t list) Or_error.t] |> print_s;
  [%expect {| (Ok ((a b c) ())) |}]

let%expect_test "string" =
  let open Let_syntax in
  let parser =
    let%bind parsed = exact_string "nyaa" ~here:[%here] in
    let%map () = eof ~here:[%here] in
    [%message "success" (parsed : string)]
  in
  parse_complete parser "nyaa"
  |> [%sexp_of: (Sexp.t * Fragment.t list) Or_error.t] |> print_s;
  [%expect
    {|
    (Ok
     ((success (parsed nyaa))
      (((label ()) (kind Debug) (message (thyme->exact_string (expected nyaa)))
        (here <opaque>) (refers_to ()) (tags ()))))) |}]

let%expect_test "choice: fail" =
  let here = [%here] in
  let parser =
    choice [ exact_string "not" ~here; exact_string "right" ~here ] ~here
  in
  parse_complete parser "different"
  |> [%sexp_of: (string * Fragment.t list) Or_error.t] |> print_s;
  [%expect
    {|
      (Error
       ("parser failed"
        (info
         (((label ()) (kind Error)
           (message
            (thyme->parse_failed (error "thyme->choice: No choices match")))
           (here <opaque>) (refers_to ()) (tags ())))))) |}]

let%expect_test "choice: fail" =
  let here = [%here] in
  let parser =
    choice
      [
        exact_string "match" ~here;
        exact_string "not" ~here;
        exact_string "right" ~here;
      ]
      ~here
  in
  parse_complete parser "match"
  |> [%sexp_of: (string * Fragment.t list) Or_error.t] |> print_s;
  [%expect
    {|
            (Ok
             (match
              (((label ()) (kind Debug) (message (thyme->exact_string (expected match)))
                (here <opaque>) (refers_to ())
                (tags
                 (((label ()) (kind Info) (message thyme->lookahead) (here <opaque>)
                   (refers_to ()) (tags ())))))
               ((label ()) (kind Debug) (message (thyme->exact_string (expected match)))
                (here <opaque>) (refers_to ()) (tags ()))))) |}]

let%expect_test "repetitions: one or more (success)" =
  let parser = one_or_more any ~here:[%here] in
  parse_complete parser "asdf"
  |> [%sexp_of: (char list * Fragment.t list) Or_error.t] |> print_s;
  [%expect {|
    (Ok ((a s d f) ())) |}]

let%expect_test "repetitions: one or more (fail)" =
  let parser = one_or_more any ~here:[%here] in
  parse_complete parser ""
  |> [%sexp_of: (char list * Fragment.t list) Or_error.t] |> print_s;
  [%expect {|
    (Error ("parser failed" (info ()))) |}]

let%expect_test "repetitions: zero or more (success)" =
  let parser = zero_or_more any ~here:[%here] in
  parse_complete parser "asdf"
  |> [%sexp_of: (char list * Fragment.t list) Or_error.t] |> print_s;
  [%expect {|
    (Ok ((a s d f) ())) |}]

let%expect_test "repetitions: zero or more (empty)" =
  let parser = zero_or_more any ~here:[%here] in
  parse_complete parser ""
  |> [%sexp_of: (char list * Fragment.t list) Or_error.t] |> print_s;
  [%expect {|
    (Ok (() ())) |}]

let%expect_test "word" =
  let parser =
    let open Let_syntax in
    let here = [%here] in
    let%bind first =
      tag (any_word ~here)
        ~tag:
          (Fragment.init Debug [%message ""] here
             ~label:"parser->first-word-in-pair")
    in
    let%bind _whitespace = zero_or_more whitespace_char ~here in
    let%bind second = any_word ~here in
    return [%message (first : string) (second : string)]
  in
  parse_complete parser "first_word second_word"
  |> [%sexp_of: (Sexp.t * Fragment.t list) Or_error.t] |> print_s;
  [%expect
    {|
    (Ok
     (((first first_word) (second second_word))
      (((label ()) (kind Debug) (message thyme->any_word) (here <opaque>)
        (refers_to ())
        (tags
         (((label (parser->first-word-in-pair)) (kind Debug) (message ())
           (here <opaque>) (refers_to ()) (tags ())))))
       ((label ()) (kind Debug) (message thyme->any_word) (here <opaque>)
        (refers_to ()) (tags ()))))) |}]
