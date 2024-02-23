open! Core
open Async

val compile : contents:string -> string Deferred.Or_error.t
