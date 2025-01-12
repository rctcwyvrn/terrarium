open! Core
open! Soil

val lex : string -> (Token.t list, Error.t) result * Fragment.t list
