open! Core
open! Soil

val lex_parse : string -> (Surface.Compilation_unit.twr, Error.t With_report.t) result
