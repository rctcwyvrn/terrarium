open! Core
open! Soil

let refer_to source pos =
  Reference.register
    (ref (source, pos))
    ~sexp_of:[%sexp_of: Source_file.t * Source_file.Position.t]
;;

module M = struct
  module Out = struct
    (**  A [Parser.t] when given a string outputs either
         - A match, with the matched value, rest of the output, and debug logs
         - No match, with debug + error logs *)

    type 'a t =
      | Match of
          { next : Source_file.Position.t
          ; v : 'a
          ; info : Fragment.t list
          }
      | No_match of Fragment.t list
    [@@deriving sexp_of]

    let tag t ~tag =
      match t with
      | Match t -> Match { t with info = List.map ~f:(Fragment.tag ~tag) t.info }
      | No_match info -> No_match (List.map ~f:(Fragment.tag ~tag) info)
    ;;
  end

  type 'a t = F of (Source_file.t -> Source_file.Position.t -> 'a Out.t)

  let run (F parse) source position = parse source position

  let bind (F parse) ~f =
    F
      (fun source position ->
        match parse source position with
        | Match { next; v; info } ->
          (* print_s [%message (info : Fragment.t list)]; *)
          (match run (f v) source next with
           | Match { next; v; info = new_info } ->
             Match { next; v; info = info @ new_info }
           | No_match new_info -> No_match (info @ new_info))
        | No_match info -> No_match info)
  ;;

  let return' ?here v =
    let info =
      match here with
      | Some loc -> [ Fragment.init Debug [%message "thyme->atom"] loc ]
      | None -> []
    in
    F (fun _source pos -> Match { next = pos; v; info })
  ;;

  let return v = return' v

  let parse_error error ~here =
    F
      (fun source pos ->
        No_match
          [ Fragment.init
              Error
              [%message "thyme->parse_failed" (error : Sexp.t)]
              here
              ~refers_to:(refer_to source pos)
          ])
  ;;

  let tag (F parse) ~tag = F (fun source pos -> parse source pos |> Out.tag ~tag)

  (* Note: the reference will refer to where the parser you added info to _stopped_ *)
  let add_info t ~frag =
    bind t ~f:(fun v ->
      F
        (fun source pos ->
          let frag = Fragment.add_ref frag (refer_to source pos) in
          Match { next = pos; v; info = [ frag ] }))
  ;;

  let map = `Define_using_bind
end

include M
include Monad.Make (M)

let any =
  F
    (fun source pos ->
      match Source_file.read_one source ~from:pos with
      | Some char, next -> Match { next; v = char; info = [] }
      | None, _ -> No_match [])
;;

let eof ~here =
  F
    (fun source pos ->
      match Source_file.read_one source ~from:pos with
      | Some char, _ ->
        No_match
          [ Fragment.init
              Error
              [%message "expected eof, got" (char : char)]
              here
              ~refers_to:(refer_to source pos)
          ]
      | None, next -> Match { next; v = (); info = [] })
;;

let sequential operations =
  let open Let_syntax in
  let%map reversed =
    List.fold operations ~init:(return []) ~f:(fun results t ->
      let%bind results = results in
      let%bind next = t in
      return (next :: results))
  in
  List.rev reversed
;;

let lookahead_matches parser =
  F
    (fun source pos ->
      match run parser source pos with
      | Match _ -> Match { next = pos; v = true; info = [] }
      (* Ignore the extra info on no_match? *)
      | No_match _info -> Match { next = pos; v = false; info = [] })
;;

(* Try the parsers from left to right, parsing the first one that consumes input

   If all parsers don't match, returns an error (without consuming input) *)
let rec choice parsers_to_try ~here =
  let open Let_syntax in
  match parsers_to_try with
  | [] -> parse_error [%message "thyme->choice: No choices match"] ~here
  | parser :: rest -> if%bind lookahead_matches parser then parser else choice rest ~here
;;

let try_ parser =
  F
    (fun source pos ->
      match run parser source pos with
      | Match { next; v; info } -> Match { next; v = Some v; info }
      (* Drop all state from stepping forward *)
      | No_match _info -> Match { next = pos; v = None; info = [] })
;;

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
;;

let zero_or_more parser ~here = choice [ one_or_more parser ~here; return [] ] ~here

let rec repeat parser ~until =
  let open Let_syntax in
  if%bind lookahead_matches until
  then (* stop repeating *)
    return []
  else (
    match%bind try_ parser with
    | None ->
      (* done *)
      return []
    | Some v ->
      let%bind rest = repeat parser ~until in
      return (v :: rest))
;;

let match_and_assert parser ~here ~pred ~pred_msg =
  let open Let_syntax in
  let%bind v = parser in
  if pred v
  then return v
  else
    parse_error
      [%message "thyme->match_and_assert: Assertion failed" (pred_msg : Sexp.t)]
      ~here
;;

let whitespace_char =
  match_and_assert
    any
    ~pred:Char.is_whitespace
    ~here:[%here]
    ~pred_msg:[%message "Char.is_whitespace"]
;;

let exact_char expected =
  match_and_assert
    any
    ~pred:(Char.equal expected)
    ~here:[%here]
    ~pred_msg:[%message "Char.equal" (expected : char)]
;;

let exact_string expected ~here =
  String.to_list expected
  |> List.map ~f:exact_char
  |> sequential
  >>| String.of_char_list
  |> add_info
       ~frag:
         (Fragment.init Debug [%message "thyme->exact_string" (expected : string)] here)
;;

let any_word ~here =
  (let open Let_syntax in
   let%map chars = repeat any ~until:whitespace_char in
   String.of_char_list chars)
  |> add_info ~frag:(Fragment.init Debug [%message "thyme->any_word"] here)
;;

let with_position_before_and_after parser =
  F
    (fun source start_pos ->
      match run parser source start_pos with
      | Match { next; v; info } -> Match { next; v = v, start_pos, next; info }
      (* Drop all state from stepping forward *)
      | No_match info -> No_match info)
;;

let parse_complete parser s =
  let source = Source_file.of_file_contents s in
  let pos = Source_file.start_of_file source in
  match run parser source pos with
  | Match { next = Source_file.Position.Eof; v; info } -> Ok v, info
  | Match { next = ended_at; v = _; info } ->
    ( Error
        (Error.create_s
           [%message
             "did not finish parsing input"
               (ended_at : Source_file.Position.t)
               (info : Fragment.t list)])
    , info )
  | No_match info ->
    Error (Error.create_s [%message "parser failed" (info : Fragment.t list)]), info
;;

type 'a parse_result = 'a Or_error.t * Fragment.t list [@@deriving sexp_of]

let%expect_test "any" =
  let source = Source_file.of_file_contents "asdf" in
  let pos = Source_file.start_of_file source in
  run any source pos |> [%sexp_of: char Out.t] |> print_s;
  [%expect {|
    (Match (next (Valid (line_num 0) (pos_in_line 1))) (v a) (info ())) |}]
;;

let%expect_test "eof" =
  let source = Source_file.of_file_contents "asdf" in
  let pos = Source_file.start_of_file source in
  run (eof ~here:[%here]) source pos |> [%sexp_of: unit Out.t] |> print_s;
  [%expect
    {|
  (No_match
   (((label ()) (kind Error) (message ("expected eof, got" (char a)))
     (here <opaque>)
     (refers_to
      ((((ref_id 0)
         (value
          (((source dummy) (file_by_line <opaque>))
           (Valid (line_num 0) (pos_in_line 0))))))))
     (tags ())))) |}]
;;

let%expect_test "eof" =
  let source = Source_file.of_file_contents "" in
  let pos = Source_file.start_of_file source in
  [%expect {| |}];
  run (eof ~here:[%here]) source pos |> [%sexp_of: unit Out.t] |> print_s;
  [%expect {| (Match (next Eof) (v ()) (info ())) |}]
;;

let%expect_test "combined" =
  let open Let_syntax in
  let parser =
    let%bind first = any in
    let%bind second = any in
    let%bind third = any in
    let%bind () = eof ~here:[%here] in
    return (first, second, third)
  in
  parse_complete parser "abc" |> [%sexp_of: (char * char * char) parse_result] |> print_s;
  [%expect {|
    ((Ok (a b c)) ()) |}]
;;

let%expect_test "string" =
  let open Let_syntax in
  let parser =
    let%bind parsed = exact_string "nyaa" ~here:[%here] in
    let%map () = eof ~here:[%here] in
    [%message "success" (parsed : string)]
  in
  parse_complete parser "nyaa" |> [%sexp_of: Sexp.t parse_result] |> print_s;
  [%expect
    {|
    ((Ok (success (parsed nyaa)))
     (((label ()) (kind Debug) (message (thyme->exact_string (expected nyaa)))
       (here <opaque>)
       (refers_to
        ((((ref_id 1) (value (((source dummy) (file_by_line <opaque>)) Eof))))))
       (tags ())))) |}]
;;

let%expect_test "choice: fail" =
  let here = [%here] in
  let parser = choice [ exact_string "not" ~here; exact_string "right" ~here ] ~here in
  parse_complete parser "different" |> [%sexp_of: string parse_result] |> print_s;
  [%expect
    {|
      ((Error
        ("parser failed"
         (info
          (((label ()) (kind Error)
            (message
             (thyme->parse_failed (error "thyme->choice: No choices match")))
            (here <opaque>)
            (refers_to
             ((((ref_id 4)
                (value
                 (((source dummy) (file_by_line <opaque>))
                  (Valid (line_num 0) (pos_in_line 0))))))))
            (tags ()))))))
       (((label ()) (kind Error)
         (message (thyme->parse_failed (error "thyme->choice: No choices match")))
         (here <opaque>)
         (refers_to
          ((((ref_id 4)
             (value
              (((source dummy) (file_by_line <opaque>))
               (Valid (line_num 0) (pos_in_line 0))))))))
         (tags ())))) |}]
;;

let%expect_test "choice: match" =
  let here = [%here] in
  let parser =
    choice
      [ exact_string "match" ~here; exact_string "not" ~here; exact_string "right" ~here ]
      ~here
  in
  parse_complete parser "match" |> [%sexp_of: string parse_result] |> print_s;
  [%expect
    {|
    ((Ok match)
     (((label ()) (kind Debug) (message (thyme->exact_string (expected match)))
       (here <opaque>)
       (refers_to
        ((((ref_id 6) (value (((source dummy) (file_by_line <opaque>)) Eof))))))
       (tags ())))) |}]
;;

let%expect_test "repetitions: one or more (success)" =
  let parser = one_or_more any ~here:[%here] in
  parse_complete parser "asdf" |> [%sexp_of: char list parse_result] |> print_s;
  [%expect {|
    ((Ok (a s d f)) ()) |}]
;;

let%expect_test "repetitions: one or more (fail)" =
  let parser = one_or_more any ~here:[%here] in
  parse_complete parser "" |> [%sexp_of: char list parse_result] |> print_s;
  [%expect {| ((Error ("parser failed" (info ()))) ()) |}]
;;

let%expect_test "repetitions: zero or more (success)" =
  let parser = zero_or_more any ~here:[%here] in
  parse_complete parser "asdf" |> [%sexp_of: char list parse_result] |> print_s;
  [%expect {| ((Ok (a s d f)) ()) |}]
;;

let%expect_test "repetitions: zero or more (empty)" =
  let parser = zero_or_more any ~here:[%here] in
  parse_complete parser "" |> [%sexp_of: char list parse_result] |> print_s;
  [%expect {| ((Ok ()) ()) |}]
;;

let%expect_test "word" =
  let parser =
    let open Let_syntax in
    let here = [%here] in
    let%bind first =
      tag
        (any_word ~here)
        ~tag:(Fragment.init Debug [%message ""] here ~label:"parser->first-word-in-pair")
    in
    let%bind _whitespace = zero_or_more whitespace_char ~here in
    let%bind second = any_word ~here in
    return [%message (first : string) (second : string)]
  in
  parse_complete parser "first_word second_word"
  |> [%sexp_of: Sexp.t parse_result]
  |> print_s;
  [%expect
    {|
    ((Ok ((first first_word) (second second_word)))
     (((label ()) (kind Debug) (message thyme->any_word) (here <opaque>)
       (refers_to
        ((((ref_id 17)
           (value
            (((source dummy) (file_by_line <opaque>))
             (Valid (line_num 0) (pos_in_line 10))))))))
       (tags
        (((label (parser->first-word-in-pair)) (kind Debug) (message ())
          (here <opaque>) (refers_to ()) (tags ())))))
      ((label ()) (kind Debug) (message thyme->any_word) (here <opaque>)
       (refers_to
        ((((ref_id 31) (value (((source dummy) (file_by_line <opaque>)) Eof))))))
       (tags ())))) |}]
;;
