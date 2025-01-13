open! Core
open! Soil

module Error : sig
  type t [@@deriving sexp_of]
end

val lex_parse : string -> (Surface.Compilation_unit.twr, Error.t With_report.t) result
