open! Core
open! Soil
open Cypress_parser
open Lexer

let%expect_test "nyaa" =
  let input =
    {| 
  module Test = struct 

    type t = { x : int ; y : int } 


    let x_coord t = t.x
    ;;

    let y_coord = fun t -> t.y
    ;;

    let coord_name = "my name"
    ;;
  end
  |}
  in
  let result = lex input in
  (match fst result with
   | Ok tokens ->
     let tokens =
       List.map ~f:Token.kind tokens
       |> List.filter ~f:(fun t -> not (Token.Kind.equal t Token.Kind.Blank))
     in
     print_s [%message (tokens : Token.Kind.t list)]
   | Error _ -> print_endline "died");
  [%expect
    {|
    (tokens
     ((Keyword Module) (Capital_ident Test) (Keyword Equal) (Keyword Struct)
      (Keyword Type) (Ident t) (Keyword Equal) (Keyword LBrace) (Ident x)
      (Keyword Colon) (Ident int) (Keyword Semicolon) (Ident y) (Keyword Colon)
      (Ident int) (Keyword RBrace) (Keyword Let) (Ident x_coord) (Ident t)
      (Keyword Equal) (Ident t) (Keyword Dot) (Ident x) (Keyword DoubleSemicolon)
      (Keyword Let) (Ident y_coord) (Keyword Equal) (Keyword Fun) (Ident t)
      (Keyword Arrow_right) (Ident t) (Keyword Dot) (Ident y)
      (Keyword DoubleSemicolon) (Keyword Let) (Ident coord_name) (Keyword Equal)
      (String "my name") (Keyword DoubleSemicolon) (Keyword End))) |}]
;;
