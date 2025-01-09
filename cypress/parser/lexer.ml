open! Core
open! Soil
open! Thyme
open Thyme.Parser.Let_syntax

let wrap_with_position parser =
  let%map kind, start, end_ = Parser.with_position_before_and_after parser in
  ({ kind; start; end_ } : Token.t)
;;

let keyword =
  (let%map keyword = Token.Keyword.lex in
   Token.Kind.Keyword keyword)
  |> wrap_with_position
;;

let blank =
  (let%map _ = Parser.one_or_more Parser.whitespace_char ~here:[%here] in
   Token.Kind.Blank)
  |> wrap_with_position
;;

(* One or more whitespace, or eof *)
let token_seperator =
  let here = [%here] in
  let eof =
    (let%map () = Parser.eof ~here in
     Token.Kind.Blank)
    |> wrap_with_position
  in
  let dot =
    (let%map _ = Parser.exact_char '.' in
     Token.Kind.Keyword Dot)
    |> wrap_with_position
  in
  let lparen =
    (let%map _ = Parser.exact_char '(' in
     Token.Kind.Keyword LParen)
    |> wrap_with_position
  in
  Parser.choice ~here [ eof; blank; dot; lparen ]
;;

let comment =
  let here = [%here] in
  let parser =
    let%bind _ = Parser.exact_string "(*" ~here in
    let comment_end = Parser.exact_string "*)" ~here in
    let%bind _ = Parser.repeat Parser.any ~until:comment_end in
    let%bind _ = comment_end in
    return Token.Kind.Comment
  in
  wrap_with_position parser
;;

let number =
  let here = [%here] in
  let parser =
    let number_char =
      Parser.match_and_assert
        Parser.any
        ~pred:Char.is_digit
        ~here
        ~pred_msg:[%message "Char.is_digit"]
    in
    let%bind start = Parser.one_or_more number_char ~here in
    if%bind Parser.lookahead_matches (Parser.exact_char '.')
    then (
      let here = [%here] in
      let%bind decimal = Parser.one_or_more number_char ~here in
      let f =
        String.of_char_list start ^ "." ^ String.of_char_list decimal |> Float.of_string
      in
      return (Token.Kind.Float f))
    else (
      let i = String.of_char_list start |> Int.of_string in
      return (Token.Kind.Integer i))
  in
  wrap_with_position parser
;;

let char =
  let parser =
    let%bind _ = Parser.exact_char '\'' in
    let%bind c = Parser.any in
    let%bind _ = Parser.exact_char '\'' in
    return (Token.Kind.Char c)
  in
  wrap_with_position parser
;;

let string =
  let parser =
    let%bind _ = Parser.exact_char '"' in
    let%bind s = Parser.repeat Parser.any ~until:(Parser.exact_char '"') in
    let%bind _ = Parser.exact_char '"' in
    return (Token.Kind.String (String.of_char_list s))
  in
  wrap_with_position parser
;;

let identifier_body_char =
  Parser.match_and_assert
    Parser.any
    ~here:[%here]
    ~pred_msg:[%message "(a...z) | (A...Z) | 0...9 | _ | '"]
    ~pred:(fun c ->
      let is_alpha_num = Char.is_alphanum c in
      let is_underscore = Char.equal c '_' in
      let is_single_quote = Char.equal c '\'' in
      is_alpha_num || is_underscore || is_single_quote)
;;

let lowercase_ident_s =
  let here = [%here] in
  let%bind first =
    Parser.match_and_assert
      Parser.any
      ~here
      ~pred:(fun c ->
        let is_lowercase_alpha = Char.is_lowercase c && Char.is_alpha c in
        let is_underscore = Char.equal c '_' in
        is_lowercase_alpha || is_underscore)
      ~pred_msg:[%message "(a...z) | _"]
  in
  let%bind body = Parser.zero_or_more identifier_body_char ~here in
  let s = String.of_char_list (first :: body) in
  return s
;;

let identifier =
  let here = [%here] in
  let parser =
    if%bind Parser.lookahead_matches lowercase_ident_s
    then (
      let%bind s = lowercase_ident_s in
      return (Token.Kind.Ident s))
    else (
      let%bind first =
        Parser.match_and_assert
          Parser.any
          ~here
          ~pred:(fun c -> Char.is_alpha c && Char.is_uppercase c)
          ~pred_msg:[%message "(A...Z)"]
      in
      let%bind body = Parser.zero_or_more identifier_body_char ~here in
      let s = String.of_char_list (first :: body) in
      return (Token.Kind.Capital_ident s))
  in
  wrap_with_position parser
;;

let arg_label =
  let parser =
    let optional_prefix = Parser.exact_char '?' in
    if%bind Parser.lookahead_matches optional_prefix
    then (
      let%bind _ = optional_prefix in
      let%bind label = lowercase_ident_s in
      return (Token.Kind.Arg_label { optional = true; label }))
    else (
      let%bind _ = Parser.exact_char '~' in
      let%bind label = lowercase_ident_s in
      return (Token.Kind.Arg_label { optional = false; label }))
  in
  wrap_with_position parser
;;

let token_with_seperator parser =
  let%bind token = parser in
  let%bind sep = token_seperator in
  return [ token; sep ]
;;

let bluebell_lexer =
  let here = [%here] in
  let token =
    [ keyword; comment; number; char; string; arg_label; identifier ]
    |> List.map ~f:token_with_seperator
    |> Parser.choice ~here
  in
  let%bind _prefix = blank in
  Parser.one_or_more token ~here >>| List.concat
;;

let lex input =
  let result = Parser.parse_complete bluebell_lexer input in
  result
;;

let%expect_test "nyaa" =
  let input =
    {| 
  module Test = struct 

    type t = { x : int ; y : int } 


    let x_coord t = t.x
    ;;

    let y_coord = fun t -> t.y
    ;;
  end
  |}
  in
  let result = lex input in
  (match fst result with
   | Ok tokens ->
     let tokens =
       List.map ~f:Token.kind tokens
       |> List.filter ~f:(fun t -> not (Token.Kind.equal t Token.Kind.Blank))
     in
     print_s [%message (tokens : Token.Kind.t list)]
   | Error _ -> print_endline "died");
  [%expect
    {|
    (tokens
     ((Keyword Module) (Capital_ident Test) (Keyword Equal) (Keyword Struct)
      (Keyword Type) (Ident t) (Keyword Equal) (Keyword LBrace) (Ident x)
      (Keyword Colon) (Ident int) (Keyword Semicolon) (Ident y) (Keyword Colon)
      (Ident int) (Keyword RBrace) (Keyword Let) (Ident x_coord) (Ident t)
      (Keyword Equal) (Ident t) (Keyword Dot) (Ident x) (Keyword DoubleSemicolon)
      (Keyword Let) (Ident y_coord) (Keyword Equal) (Keyword Fun) (Ident t)
      (Keyword Arrow_right) (Ident t) (Keyword Dot) (Ident y)
      (Keyword DoubleSemicolon) (Keyword End))) |}]
;;
(* print_s [%message (lex input : Token.t list Or_error.t * Fragment.t list)]; *)
