open! Core

(** A [Reference.t] is something that can be referred to in an error/fragment *)

type t [@@deriving sexp_of]

val register : 'a Ref.t -> sexp_of:('a -> Sexp.t) -> t
val get : t -> Sexp.t
