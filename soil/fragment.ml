open! Core

module Kind = struct
  type t = Debug | Info | Error | Important [@@deriving sexp_of]
end

type t = {
  label : string option;
  kind : Kind.t;
  message : Sexp.t;
  (* fixme-soon: need to make a thing for testing vs real? or maybe just no opaque is fine? *)
  here : (Source_code_position.t[@sexp.opaque]);
  refers_to : Reference.t option;
  tags : t list;
}
[@@deriving sexp_of]

let init ?label ?refers_to kind message here =
  { label; kind; message; here; refers_to; tags = [] }

let tag t ~tag = { t with tags = tag :: t.tags }
