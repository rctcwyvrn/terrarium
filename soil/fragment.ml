open! Core

module Kind = struct
  type t = Debug | Info | Error | Important [@@deriving sexp_of]
end

type t = {
  label : string option;
  kind : Kind.t;
  message : Sexp.t;
  here : Source_code_position.t;
  refers_to : Reference.t option;
}
[@@deriving sexp_of]

let init ?label ?refers_to kind message here =
  { label; kind; message; here; refers_to }
