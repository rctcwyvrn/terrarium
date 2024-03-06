open! Core

module M = struct
  type 'a t =
    { maybe_val : 'a option
    ; info : Fragment.t list
    }
  [@@deriving sexp_of]

  let bind { maybe_val; info } ~f =
    match maybe_val with
    | Some v ->
      let { maybe_val; info = new_info } = f v in
      { maybe_val; info = info @ new_info }
    | None -> { maybe_val = None; info }
  ;;

  let return v = { maybe_val = Some v; info = [] }
  let map = `Define_using_bind
end

include M
include Monad.Make (M)

let add { maybe_val; info } fragment = { maybe_val; info = fragment :: info }
