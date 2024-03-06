open! Core

module Position : sig
  type t =
    | Valid of
        { line_num : int
        ; pos_in_line : int
        }
    | Eof
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val read_one : t -> from:Position.t -> char option * Position.t
val of_file_contents : ?source:File_path.t -> string -> t
val start_of_file : t -> Position.t
