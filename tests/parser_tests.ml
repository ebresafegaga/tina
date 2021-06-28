open Parser

module G = Grammar

(* useful functions *)
let tokenize str =
  let lb = Lexing.from_string str in
  let rec loop () =
    match Lexer.read_token lb with
    | Grammar.EOF -> []
    | token -> token :: loop ()
    | exception Lexer.SyntaxError _ -> [] (* for now *)
  in
  loop ()

let token_testable =
  let pp formatter token = Format.fprintf formatter "%s" (ParserEntry.pp_token token) in
  Alcotest.testable pp (=)

let token_test str token error () =  
  Alcotest.check (Alcotest.list token_testable) error
    [token]
    (tokenize str)

let lex_claim_test = token_test "claim" G.CLAIM "claim should result in token CLAIM"
let lex_claim_text_case =
  "claim-case", [Alcotest.test_case "lex claim only" `Quick lex_claim_test]

let lex_int_test = token_test "234" (G.INT 234) "integer should result in INT 234"
let lex_int_test2 = token_test "92333" (G.INT 92333) "integer should result in INT 92333"
let lex_int_test3 = token_test "-89" (G.INT (-89)) "integer should result in INT -89"
let lex_int_test_case =
  "int-token-case",
  [Alcotest.test_case "lex int token 234" `Quick lex_int_test;
   Alcotest.test_case "lex int token 92333" `Quick lex_int_test2;
   Alcotest.test_case "lex int token -89" `Quick lex_int_test3 ]

let lex_float_test = token_test "2.18282" (G.FLOAT 2.18282) "float should result in FLOAT 2.18282"
let lex_float_test2 = token_test "3.142" (G.FLOAT 3.142) "float should result in FLOAT 3.142"
let lex_float_test3 = token_test "-17.29" (G.FLOAT (-17.29)) "float should result in FLOAT -17.29"
let lex_float_test_case =
  "float-token-case",
  [Alcotest.test_case "lex float token 2.18282" `Quick lex_float_test;
   Alcotest.test_case "lex float token 18282" `Quick lex_float_test2;
   Alcotest.test_case "lex float token -17.29" `Quick lex_float_test3;]

let all_test_cases =
  [lex_claim_text_case;
   lex_int_test_case;
   lex_float_test_case]
