open! Core

module Keyword : sig
  (* things i might want to include
     - some of the other operators (lsl??? lsr?)
     - private
     - assert
     - external *)
  (* Boolean things *)
  type t =
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
  [@@deriving sexp_of, enumerate, equal]

  val lex : t Thyme.Parser.t
end

module Kind : sig
  module Argless : sig
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

  (* notes on things that arent here
     - literal variants (integer,float,char,string)
     - prefix/infix special cases *)
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

val is_keyword : t -> Keyword.t -> bool
val has_kind : t -> Kind.Argless.t -> bool
val take_exn : t -> string
