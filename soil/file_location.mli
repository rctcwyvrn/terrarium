open! Core

(** A [File_location.t] specifies a location in a non-ocaml file *)

type t [@@deriving sexp_of]

val init : path:File_path.t -> row:int -> col:int -> t
