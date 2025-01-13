open! Core
open! Soil
open Cypress_parser

let%expect_test "nyaa" =
  let _ = Parser.lex_parse Test_program.input in
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  yeet
  Raised at Base__Error.raise in file "src/error.ml" (inlined), line 9, characters 14-30
  Called from Base__Error.raise_s in file "src/error.ml", line 10, characters 19-40
  Called from Cypress_parser_tests__Test_parser.(fun) in file "cypress/parser/test/test_parser.ml", line 6, characters 10-45
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  (("Fragment.message frag" (thyme->exact_string (expected module)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected module)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected module)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected module)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected struct)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected struct)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected struct)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected struct)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected type)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected type)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected type)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected type)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected {)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected {)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected {)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected {)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected :)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ->)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ->)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ->)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ->)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected in)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected })))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected })))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected })))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected })))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected fun)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected fun)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected fun)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected fun)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ->)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ->)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ->)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ->)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected .)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected "(")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected "(")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected "(")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected "(")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ")")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ")")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ")")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ")")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected let)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" cypress->identifier)
   ("Fragment.here frag" cypress/parser/src/lexer.ml:133:13))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected =)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected ";;")))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected end)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected end)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected end)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  ---
  (("Fragment.message frag" (thyme->exact_string (expected end)))
   ("Fragment.here frag" cypress/parser/src/token.ml:56:15))
  --- |}]
;;
