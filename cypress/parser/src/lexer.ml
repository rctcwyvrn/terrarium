open! Core
open! Soil
open Thyme.Parser.Let_syntax

let wrap_with_position parser =
  let%map kind, start, end_ = Thyme.Parser.with_position_before_and_after parser in
  ({ kind; start; end_ } : Token.t)
;;

let keyword =
  (let%map keyword = Token.Keyword.lex in
   Token.Kind.Keyword keyword)
  |> wrap_with_position
;;

let blank =
  (let%map _ = Thyme.Parser.zero_or_more Thyme.Parser.whitespace_char ~here:[%here] in
   Token.Kind.Blank)
  |> wrap_with_position
;;

(* One or more whitespace, or eof *)
let token_seperator =
  let here = [%here] in
  let eof =
    (let%map () = Thyme.Parser.eof ~here in
     Token.Kind.Blank)
    |> wrap_with_position
  in
  let dot =
    (let%map _ = Thyme.Parser.exact_char '.' in
     Token.Kind.Keyword Dot)
    |> wrap_with_position
  in
  let lparen =
    (let%map _ = Thyme.Parser.exact_char '(' in
     Token.Kind.Keyword LParen)
    |> wrap_with_position
  in
  Thyme.Parser.choice ~here [ eof; blank; dot; lparen ]
;;

let comment =
  let here = [%here] in
  let parser =
    let%bind _ = Thyme.Parser.exact_string "(*" ~here in
    let comment_end = Thyme.Parser.exact_string "*)" ~here in
    let%bind _ = Thyme.Parser.repeat Thyme.Parser.any ~until:comment_end in
    let%bind _ = comment_end in
    return Token.Kind.Comment
  in
  wrap_with_position parser
;;

let number =
  let here = [%here] in
  let parser =
    let number_char =
      Thyme.Parser.match_and_assert
        Thyme.Parser.any
        ~pred:Char.is_digit
        ~here
        ~pred_msg:[%message "Char.is_digit"]
    in
    let%bind start = Thyme.Parser.one_or_more number_char ~here in
    if%bind Thyme.Parser.lookahead_matches (Thyme.Parser.exact_char '.')
    then (
      let here = [%here] in
      let%bind decimal = Thyme.Parser.one_or_more number_char ~here in
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
    let%bind _ = Thyme.Parser.exact_char '\'' in
    let%bind c = Thyme.Parser.any in
    let%bind _ = Thyme.Parser.exact_char '\'' in
    return (Token.Kind.Char c)
  in
  wrap_with_position parser
;;

let string =
  let parser =
    let%bind _ = Thyme.Parser.exact_char '"' in
    let%bind s =
      Thyme.Parser.repeat Thyme.Parser.any ~until:(Thyme.Parser.exact_char '"')
    in
    let%bind _ = Thyme.Parser.exact_char '"' in
    return (Token.Kind.String (String.of_char_list s))
  in
  wrap_with_position parser
;;

let identifier_body_char =
  Thyme.Parser.match_and_assert
    Thyme.Parser.any
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
    Thyme.Parser.match_and_assert
      Thyme.Parser.any
      ~here
      ~pred:(fun c ->
        let is_lowercase_alpha = Char.is_lowercase c && Char.is_alpha c in
        let is_underscore = Char.equal c '_' in
        is_lowercase_alpha || is_underscore)
      ~pred_msg:[%message "(a...z) | _"]
  in
  let%bind body = Thyme.Parser.zero_or_more identifier_body_char ~here in
  let s = String.of_char_list (first :: body) in
  return s
;;

let identifier =
  let here = [%here] in
  let parser =
    if%bind Thyme.Parser.lookahead_matches lowercase_ident_s
    then (
      let%bind s = lowercase_ident_s in
      return (Token.Kind.Ident s))
    else (
      let%bind first =
        Thyme.Parser.match_and_assert
          Thyme.Parser.any
          ~here
          ~pred:(fun c -> Char.is_alpha c && Char.is_uppercase c)
          ~pred_msg:[%message "(A...Z)"]
      in
      let%bind body = Thyme.Parser.zero_or_more identifier_body_char ~here in
      let s = String.of_char_list (first :: body) in
      return (Token.Kind.Capital_ident s))
  in
  wrap_with_position parser
  |> Thyme.Parser.add_info
       ~frag:(Fragment.init Debug [%message "cypress->identifier"] here)
;;

let arg_label =
  let parser =
    let optional_prefix = Thyme.Parser.exact_char '?' in
    if%bind Thyme.Parser.lookahead_matches optional_prefix
    then (
      let%bind _ = optional_prefix in
      let%bind label = lowercase_ident_s in
      return (Token.Kind.Arg_label { optional = true; label }))
    else (
      let%bind _ = Thyme.Parser.exact_char '~' in
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

let cypress_lexer =
  let here = [%here] in
  let token =
    [ keyword; comment; number; char; string; arg_label; identifier ]
    |> List.map ~f:token_with_seperator
    |> Thyme.Parser.choice ~here
  in
  let%bind _prefix = blank in
  let%bind results = Thyme.Parser.one_or_more token ~here >>| List.concat in
  let%bind _suffix = blank in
  return results
;;

let lex input = Thyme.Parser.parse_complete cypress_lexer input
