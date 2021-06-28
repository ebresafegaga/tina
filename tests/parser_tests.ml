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
    
let lex_claim_test_case =
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
    
let lex_float_test4 = token_test ".227" (G.FLOAT (0.227)) "float should result in FLOAT .227"
    
let lex_float_test5 = token_test "0.0023" (G.FLOAT 0.0023) "float should result in FLOAT 0.0023"
    
let lex_float_test_case =
  "float-token-case",
  [Alcotest.test_case "lex float token 2.18282" `Quick lex_float_test;
   Alcotest.test_case "lex float token 18282" `Quick lex_float_test2;
   Alcotest.test_case "lex float token -17.29" `Quick lex_float_test3;
   Alcotest.test_case "lex float token .227" `Quick lex_float_test4;
   Alcotest.test_case "lex float token 0.0023" `Quick lex_float_test5]

let lex_id_test = token_test "simple" (G.ID "simple") "id should result in ID simple"

let lex_id_test2 = token_test "_bASic12" (G.ID "_bASic12") "id should result in ID _bASic12"
    
let lex_id_test_case =
  "id-token-case",
  [Alcotest.test_case "lex id token simple" `Quick lex_id_test;
   Alcotest.test_case "lex id token _bASic12" `Quick lex_id_test2]

let lex_string_test = token_test {|"fancy str"|} (G.STRING {|"fancy str"|}) "string should result in STRING \"fancy str\""

let lex_string_test_case =
  "string-token-case",
  [Alcotest.test_case "lex string token \"fancy str\"" `Quick lex_id_test]

let lex_bool_test = token_test {|true|} G.TRUE "bool true should result in the token TRUE"

let lex_bool_test2 = token_test {|false|} G.FALSE "bool false should result in the token FALSE"

let lex_bool_test_case =
  "bool-token-case",
  [Alcotest.test_case "lex true token" `Quick lex_bool_test;
   Alcotest.test_case "lex false token" `Quick lex_bool_test2]


let lex_def_test = token_test "def" G.DEF "def should result in token DEF"
    
let lex_def_test_case =
  "def-case", [Alcotest.test_case "lex def token" `Quick lex_def_test]


let lex_datatype_test = token_test "datatype" G.DATA "datatype should result in token DATA"
    
let lex_datatype_test_case =
  "datatype-case", [Alcotest.test_case "lex datatype keyword" `Quick lex_datatype_test]


let all_test_cases =
  [lex_claim_test_case;
   lex_int_test_case;
   lex_float_test_case;
   lex_id_test_case;
   lex_string_test_case;
   lex_bool_test_case;
   lex_def_test_case;
   lex_datatype_test_case]
