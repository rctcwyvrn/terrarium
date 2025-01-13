open! Core

module Kind = struct
  type t =
    | Debug
    | Info
    | Error
    | Important
  [@@deriving sexp_of]
end

type t =
  { label : string option
  ; kind : Kind.t
  ; message : Sexp.t
  ; (* fixme-soon: need to make a thing for testing vs real? or maybe just no opaque is fine? *)
    here : (Source_code_position.t[@sexp.opaque])
  ; refers_to : Reference.t list option
  ; tags : t list
  }
[@@deriving sexp_of, fields ~getters]

let init ?label ?refers_to kind message here =
  { label
  ; kind
  ; message
  ; here
  ; refers_to = Option.map refers_to ~f:(fun ref -> [ ref ])
  ; tags = []
  }
;;

let tag t ~tag = { t with tags = tag :: t.tags }

let add_ref t ref =
  let new_refers_to =
    match t.refers_to with
    | Some refs -> Some (ref :: refs)
    | None -> Some [ ref ]
  in
  { t with refers_to = new_refers_to }
;;
