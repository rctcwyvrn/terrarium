open! Core

type t =
  { path : File_path.t
  ; row : int
  ; col : int
  }
[@@deriving sexp]

let init ~path ~row ~col = { path; row; col }
