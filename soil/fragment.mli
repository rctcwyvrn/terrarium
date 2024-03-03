open! Core

(** A [Fragment.t] is a generic little bit of debug information *)

module Kind : sig
  type t = Debug | Info | Error | Important [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val init :
  ?label:string ->
  ?message:Sexp.t ->
  ?here:Source_code_position.t ->
  ?refers_to:Reference.t ->
  Kind.t ->
  t
