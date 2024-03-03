open! Core

module Kind = struct
  type t = Debug | Info | Error | Important [@@deriving sexp_of]
end

type t = {
  label : string option;
  kind : Kind.t;
  message : Sexp.t option;
  here : Source_code_position.t option;
  refers_to : Reference.t option;
}
[@@deriving sexp_of]

let init ?label ?message ?here ?refers_to kind =
  { label; kind; message; here; refers_to }
