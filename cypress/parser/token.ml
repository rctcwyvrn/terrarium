open! Core

module Keyword = struct
  type t =
    (* Boolean things *)
    | If
    | Else
    | Logical_and
    | Logical_or
    | True
    | False
    (* Match *)
    | Match
    | With
    (* Let bindings *)
    | Let
    | Rec
    | And
    | In
    (* module things *)
    | Module
    | Struct
    | Functor
    | End
    | Open
    (* type *)
    | Type
    (* function things *)
    | Fun
    (* Symbols *)
    | Equal
    | LParen
    | RParen
    | LBrace
    | RBrace
    | Colon_equal
    | ColonColon
    | Colon
    | DoubleSemicolon
    | Semicolon
    | Comma
    | Exclamation
    | Star
    | Plus
    | Dash
    | Arrow_right
    | Pipe_lt
    | Pipe
    | Dot
  [@@deriving enumerate, sexp_of, variants, equal]
  (* fixme-soon: total map???? *)

  let lex =
    let open Thyme.Parser in
    let open Thyme.Parser.Let_syntax in
    let here = [%here] in
    let s s = exact_string s ~here in
    let into keyword parser =
      let%map _ = parser in
      keyword
    in
    choice
      ~here
      [ s "if" |> into If
      ; s "else" |> into Else
      ; s "&&" |> into Logical_and
      ; s "||" |> into Logical_or
      ; s "true" |> into True
      ; s "false" |> into False
      ; s "match" |> into Match
      ; s "with" |> into With
      ; s "let" |> into Let
      ; s "rec" |> into Rec
      ; s "and" |> into And
      ; s "in" |> into In
      ; s "module" |> into Module
      ; s "struct" |> into Struct
      ; s "functor" |> into Functor
      ; s "end" |> into End
      ; s "open" |> into Open
      ; s "type" |> into Type
      ; s "fun" |> into Fun
      ; s "=" |> into Equal
      ; s "(" |> into LParen
      ; s ")" |> into RParen
      ; s "{" |> into LBrace
      ; s "}" |> into RBrace
      ; s ":=" |> into Colon_equal
      ; s "::" |> into ColonColon
      ; s ":" |> into Colon
      ; s ";;" |> into DoubleSemicolon
      ; s ";" |> into Semicolon
      ; s "," |> into Comma
      ; s "!" |> into Exclamation
      ; s "*" |> into Star
      ; s "+" |> into Plus
      ; s "->" |> into Arrow_right
      ; s "-" |> into Dash
      ; s "|>" |> into Pipe_lt
      ; s "|" |> into Pipe
      ; s "." |> into Dot
      ]
  ;;
end

module Kind = struct
  type t =
    | Blank
    | Comment
    | Ident of string
    | Capital_ident of string
    | Integer of int
    | Float of float
    | Char of char
    | String of string
    | Arg_label of
        { optional : bool
        ; label : string
        }
    | Keyword of Keyword.t
  [@@deriving sexp_of, equal]
end

type t =
  { kind : Kind.t
  ; start : Thyme.Source_file.Position.t
  ; end_ : Thyme.Source_file.Position.t
  }
[@@deriving sexp_of, fields ~getters]
