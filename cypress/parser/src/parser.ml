open! Core
open! Soil

type t =
  { toks : Token.t list
  ; frags : Fragment.t list
  }

let parse t = compilation_unit t

let lex_parse s =
  let toks_or_error, frags = Lexer.lex s in
  let%bind.Result toks = toks_or_error in
  parse { toks; frags }
;;
