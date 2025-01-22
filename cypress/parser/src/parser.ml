open! Core
open! Soil
open! Token
open Result.Let_syntax

module Error = struct
  type t =
    | Unexpected_eof
    | Extra_tokens of { tokens : Token.t list }
    | Bad_keyword of
        { tok : Token.t
        ; expected : Token.Keyword.t
        }
    | Bad_kind of
        { tok : Token.t
        ; expected : Token.Kind.Argless.t
        }
    | Lex_error of Error.t
  [@@deriving sexp_of]

  let lexer error = Lex_error error
end

type t = { toks : (Token.t * Fragment.t) list }

let expect t kind =
  match t.toks with
  | [] -> Error (With_report.return Error.Unexpected_eof)
  | (tok, frag) :: rest ->
    if Token.has_kind tok kind
    then Ok ({ toks = rest }, tok)
    else
      Error
        (With_report.return (Error.Bad_kind { tok; expected = kind })
         |> With_report.add ~frag)
;;

let expect_keyword t keyword =
  match t.toks with
  | [] -> Error (With_report.return Error.Unexpected_eof)
  | (tok, frag) :: rest ->
    if Token.is_keyword tok keyword
    then Ok { toks = rest }
    else
      Error
        (With_report.return (Error.Bad_keyword { tok; expected = keyword })
         |> With_report.add ~frag)
;;

let expect_eof t =
  match t.toks with
  | [] -> Ok ()
  | tokens ->
    Error (With_report.return (Error.Extra_tokens { tokens = List.map ~f:fst tokens }))
;;

let take result take_fn =
  let%map t, token = result in
  t, take_fn token
;;

let zero_or_more f t =
  ignore f;
  ignore t;
  raise_s [%message "unimplemented"]
;;

let specification t =
  ignore t;
  raise_s [%message "unimplemented"]
;;

let module_item t =
  ignore t;
  raise_s [%message "unimplemented"]
;;

(* fixme: i should make this monadic *)
let compilation_unit t =
  (* fixme: functors are not supported lmao *)
  let%bind t = expect_keyword t Module in
  let%bind t, module_name = take (expect t Capital_ident) take_exn in
  let%bind t = expect_keyword t Colon in
  let%bind t = expect_keyword t Sig in
  let%bind t, interface = zero_or_more specification t in
  let%bind t = expect_keyword t End in
  let%bind t = expect_keyword t Equal in
  let%bind t = expect_keyword t Struct in
  let%bind t, impl = zero_or_more module_item t in
  let%bind t = expect_keyword t End in
  let%bind () = expect_eof t in
  ignore module_name;
  Ok (With_report.return { Surface.Compilation_unit.interface; impl })
;;

let parse t = compilation_unit t

let lex_parse s =
  let toks_or_error, frags = Lexer.lex s in
  let%bind toks =
    toks_or_error
    |> Result.map_error ~f:Error.lexer
    |> Result.map_error ~f:With_report.return
  in
  (* List.iter ~f:(fun { kind; _ } -> print_s [%message (kind : Kind.t)]) toks; *)
  parse { toks = List.zip_exn toks frags }
;;
