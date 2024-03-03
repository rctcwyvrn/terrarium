open! Core

module Entry = struct
  type t = T : { value_ptr : 'a Ref.t; sexp_of : 'a -> Sexp.t } -> t
end

let registered = Queue.create ()

type t = int

let sexp_of_t ref_id = [%message (ref_id : int)]

let register value_ptr ~sexp_of =
  let id = Queue.length registered in
  Queue.enqueue registered (Entry.T { value_ptr; sexp_of });
  id

let get ref_id =
  let (T { value_ptr; sexp_of }) = Queue.get registered ref_id in
  sexp_of !value_ptr
