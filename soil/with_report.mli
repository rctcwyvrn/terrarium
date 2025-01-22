open! Core

(** A ['a With_report.t] is Some 'a or None + fragments indicating debugging logs or errors *)

type 'a t [@@deriving sexp_of]

include Monad.S with type 'a t := 'a t

val add : 'a t -> frag:Fragment.t -> 'a t
