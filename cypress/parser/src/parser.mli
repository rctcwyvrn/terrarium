open! Core
open! Soil

val lex_parse
  :  string
  -> (Surface.Compilation_unit.t_with_report, Error.t With_report.t) result
