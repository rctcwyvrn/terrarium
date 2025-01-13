open! Core
open! Soil
open Cypress_parser
open Lexer

let%expect_test "nyaa" =
  let result = lex Test_program.input in
  (match fst result with
   | Ok tokens ->
     let tokens =
       List.map ~f:Token.kind tokens
       |> List.filter ~f:(fun t -> not (Token.Kind.equal t Token.Kind.Blank))
     in
     print_s [%message (tokens : Token.Kind.t list)]
   | Error e -> print_s [%message "died" (e : Error.t)]);
  [%expect
    {|
    (tokens
     ((Keyword Module) (Capital_ident Test) (Keyword Equal) (Keyword Struct)
      (Keyword Type) (Ident t) (Keyword Equal) (Keyword LBrace) (Ident x)
      (Keyword Colon) (Keyword In) (Ident t) (Keyword Semicolon) (Ident y)
      (Keyword Colon) (Keyword In) (Ident t) (Keyword Semicolon) (Ident f)
      (Keyword Colon) (Ident unit) (Keyword Arrow_right) (Keyword In) (Ident t)
      (Keyword RBrace) (Keyword Let) (Ident x_coord) (Ident t) (Keyword Equal)
      (Ident t) (Keyword Dot) (Ident x) (Keyword DoubleSemicolon) (Keyword Let)
      (Ident y_coord) (Keyword Equal) (Keyword Fun) (Ident t)
      (Keyword Arrow_right) (Ident t) (Keyword Dot) (Ident y)
      (Keyword DoubleSemicolon) (Keyword Let) (Ident apply) (Ident t)
      (Keyword Equal) (Ident t) (Keyword Dot) (Ident f) (Keyword LParen)
      (Keyword RParen) (Keyword DoubleSemicolon) (Keyword Let) (Ident coord_name)
      (Keyword Equal) (String "my name") (Keyword DoubleSemicolon) (Keyword End))) |}]
;;
