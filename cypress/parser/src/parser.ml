open! Core
open! Soil
open! Token

module Error = struct
  type t =
    | Unexpected_eof
    | Lex_error of Error.t
  [@@deriving sexp_of]

  let lexer error = Lex_error error
end

type t =
  { toks : Token.t list
  ; frags : Fragment.t list
  }

let compilation_unit t =
  ignore t;
  ignore t.toks;
  ignore t.frags;
  ignore Error.Unexpected_eof;
  raise_s [%message "yeet"]
;;

let parse t = compilation_unit t

let lex_parse s =
  let toks_or_error, frags = Lexer.lex s in
  let%bind.Result toks =
    toks_or_error
    |> Result.map_error ~f:Error.lexer
    |> Result.map_error ~f:With_report.return
  in
  List.iter frags ~f:(fun frag ->
    print_s
      [%message
        (Fragment.message frag : Sexp.t) (Fragment.here frag : Source_code_position.t)];
    print_endline "---");
  parse { toks; frags }
;;
