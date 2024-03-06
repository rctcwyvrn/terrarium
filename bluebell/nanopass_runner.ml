open! Core
open Async

let compile ~contents =
  print_s [%message "compiling" (contents : string)];
  Deferred.Or_error.return contents
;;
