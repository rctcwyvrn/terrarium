open! Core

(** A [Fragment.t] is a generic little bit of debug information *)

module Kind : sig
  type t =
    | Debug
    | Info
    | Error
    | Important
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val init
  :  ?label:string
  -> ?refers_to:Reference.t
  -> Kind.t
  -> Sexp.t
  -> Source_code_position.t
  -> t

val tag : t -> tag:t -> t
val add_ref : t -> Reference.t -> t
val message : t -> Sexp.t
val here : t -> Source_code_position.t
