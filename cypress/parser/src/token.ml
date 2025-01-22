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
    | Sig
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
      ; s "sig" |> into Sig
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
  module Argless = struct
    type t =
      | Blank
      | Comment
      | Ident
      | Capital_ident
      | Integer
      | Float
      | Char
      | String
      | Arg_label
      | Keyword
    [@@deriving sexp_of, equal]
  end

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

let is_keyword { kind; _ } k1 =
  match kind with
  | Keyword k2 -> Keyword.equal k1 k2
  | _ -> false
;;

let has_kind { kind; _ } k1 =
  let k2 =
    match kind with
    | Blank -> Kind.Argless.Blank
    | Comment -> Comment
    | Ident _ -> Ident
    | Capital_ident _ -> Capital_ident
    | Integer _ -> Integer
    | Float _ -> Float
    | Char _ -> Char
    | String _ -> String
    | Arg_label _ -> Arg_label
    | Keyword _ -> Keyword
  in
  Kind.Argless.equal k1 k2
;;

let take_exn { kind; _ } =
  match kind with
  | Ident s | Capital_ident s | String s -> s
  | _ -> raise_s [%message "wtf you didnt check"]
;;
