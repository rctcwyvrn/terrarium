open! Core
open! Soil

(** A monadic parser with nice error handling *)

val refer_to : Source_file.t -> Source_file.Position.t -> Reference.t

type 'a t

include Monad.S with type 'a t := 'a t

val parse_error : Sexplib0.Sexp.t -> here:Lexing.position -> 'a t
val tag : 'a t -> tag:Fragment.t -> 'a t
val add_info : 'a t -> frag:Fragment.t -> 'a t

val sequential : 'a t list -> 'a list t
(** Sequencing operation *)

val lookahead_matches : 'a t -> bool t
(** Peek ahead and see if the parser matches

   Never consumes *)

val choice : 'a t list -> here:Lexing.position -> 'a t
(** Tries the given parsers in order, accepting the first one that matches *)

val try_ : 'a t -> 'a option t
(** Tries the given parser, returning whether or not it succeeded in the form of an option *)

(** Repetition *)

val one_or_more : 'a t -> here:Lexing.position -> 'a list t
val zero_or_more : 'a t -> here:Lexing.position -> 'a list t
val repeat : 'a t -> until:'b t -> 'a list t

val match_and_assert :
  'a t ->
  here:Lexing.position ->
  pred:('a -> bool) ->
  pred_msg:Sexplib0.Sexp.t ->
  'a t
(** Adds a predicate to the given parser *)

(** Common parsers *)

val any : char t
val eof : here:Lexing.position -> unit t
val whitespace_char : char t
val exact_char : char -> char t
val exact_string : string -> here:Lexing.position -> string t
val any_word : here:Lexing.position -> string t

(* fixme-soon: this should probably take a file_path *)
val parse_complete : 'a t -> string -> ('a * Fragment.t list, Error.t) result
(** Parse a string completely *)
