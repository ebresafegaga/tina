open Parser

module G = Grammar
module P = ParserEntry  

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
  let pp formatter token = Format.fprintf formatter "%s" (P.pp_token token) in
  Alcotest.testable pp (=)

let token_test str token error () =  
  Alcotest.check (Alcotest.list token_testable) error
    [token]
    (tokenize str)

let token_test_case case_name test_list =
  let tests =
    test_list
    |> List.map (fun (name, test) -> Alcotest.test_case name `Quick test)
  in
  case_name, tests

let token_test str token () =
  let msg = Printf.sprintf "input %s should result in token %s" str (P.pp_token token) in
  Alcotest.check (Alcotest.list token_testable) msg
    [token]
    (tokenize str)

let token_list_test str tokens () =
  let recieved_tokens = tokenize str in
  List.combine recieved_tokens tokens
  |> List.iter (fun (recieved, actual) ->
      let msg =
        Printf.sprintf "Expected %s, but got %s"
          (P.pp_token recieved)
          (P.pp_token actual)
      in
      Alcotest.check token_testable msg recieved actual)

let lex_claim_test = token_test "claim" G.CLAIM 
    
let lex_claim_test_case = token_test_case "claim-case" [("lex claim only", lex_claim_test)]

let lex_int_test = token_test "234" (G.INT 234) 

let lex_int_test2 = token_test "92333" (G.INT 92333) 
    
let lex_int_test3 = token_test "-89" (G.INT (-89)) 
    
let lex_int_test_case =
  token_test_case "int-token-case"
    ["lex int token 234", lex_int_test;
     "lex int token 92333", lex_int_test2;
     "lex int token -89", lex_int_test3 ]

let lex_float_test = token_test "2.18282" (G.FLOAT 2.18282) 
    
let lex_float_test2 = token_test "3.142" (G.FLOAT 3.142) 
    
let lex_float_test3 = token_test "-17.29" (G.FLOAT (-17.29))     
let lex_float_test4 = token_test ".227" (G.FLOAT (0.227)) 
    
let lex_float_test5 = token_test "0.0023" (G.FLOAT 0.0023)
    
let lex_float_test_case =
  token_test_case "float-token-case"
    [ "lex float token 2.18282",  lex_float_test;
      "lex float token 18282", lex_float_test2;
      "lex float token -17.29",  lex_float_test3;
      "lex float token .227", lex_float_test4;
      "lex float token 0.0023", lex_float_test5]

let lex_id_test = token_test "simple" (G.ID "simple") 

let lex_id_test2 = token_test "_bASic12" (G.ID "_bASic12")
    
let lex_id_test_case =
  token_test_case "id-token-case"
  [ "lex id token simple", lex_id_test;
    "lex id token _bASic12", lex_id_test2]

let lex_string_test = token_test {|"fancy str"|} (G.STRING {|"fancy str"|})

let lex_string_test_case =
  token_test_case "string-token-case"
    ["lex string token \"fancy str\"" , lex_id_test]

let lex_bool_test = token_test {|true|} G.TRUE 

let lex_bool_test2 = token_test {|false|} G.FALSE 

let lex_bool_test_case =
  token_test_case "bool-token-case"
    [ "lex true token", lex_bool_test;
      "lex false token", lex_bool_test2]


let lex_def_test = token_test "def" G.DEF 
    
let lex_def_test_case =
 token_test_case  "def-case" ["lex def token", lex_def_test]


let lex_datatype_test = token_test "datatype" G.DATA 
    
let lex_datatype_test_case =
 token_test_case "datatype-case" ["lex datatype keyword", lex_datatype_test]


let lex_case_test = token_test "case" G.CASE
    
let lex_case_test_case =
 token_test_case "case-tokens-case" ["lex case token", lex_case_test]

let lex_ability_test = token_test "ability" G.ABILITY 
    
let lex_ability_test_case =
  token_test_case "ability-case" ["lex ability token", lex_ability_test]

let lex_let_test = token_test "let" G.LET
    
let lex_let_test_case =
  token_test_case "let-case" ["lex let token", lex_let_test]

let lex_fn_test = token_test "fn" G.FN 
    
let lex_fn_test_case =
  token_test_case "fn-case" ["lex fn token", lex_fn_test]

let lex_mut_test = token_test "mut" G.MUT
    
let lex_mut_test_case =
  token_test_case "mut-case" ["lex mut token", lex_mut_test]

let lex_end_test = token_test "end" G.END
    
let lex_end_test_case =
  token_test_case "end-case" ["lex end token", lex_end_test]


let lex_if_test = token_test "if" G.IF
    
let lex_if_test_case =
  token_test_case "if-case" ["lex if token", lex_if_test]

let lex_else_test = token_test "else" G.ELSE
    
let lex_else_test_case =
  token_test_case "else-case" ["lex else token", lex_else_test]              

let lex_then_test = token_test "then" G.THEN
let lex_then_test_case =
  token_test_case "then-case" ["lex then token", lex_then_test]

let program_test =
  let sample = {| 
    def test =
    let Person {age:a, other:o } = gaga;
    case (a) {
    10 -> "no"
    }
   |}
  in
  token_list_test sample
    [ G.DEF; G.ID "test"; G.EQUALS; G.LET; G.ID "Person";
      G.LBRACE; G.ID "age"; G.COLON; G.ID "a" ; G.COMMA;
      G.ID "other"; G.COLON; G.ID "o"; G.RBRACE;
      G.EQUALS; G.ID "gaga" ; G.SEMICOLON;
      G.CASE; G.LPAREN; G.ID "a"; G.RPAREN; G.LBRACE;
      G.INT 10; G.ARROW; G.STRING "no" ; G.RBRACE]

let program_test_case =
  token_test_case "program-case"
    ["test def, case, ids, string, int used in a program", program_test]

let operators_test =
  let sample = "><+>=<=-*/->{}[]()." in
  token_list_test sample
    [G.GT; G.LT; G.PLUS; G.GTEQUALS; G.LTEQUALS;
     G.MINUS; G.STAR; G.DIV; G.ARROW; G.LBRACE; G.RBRACE;
     G.LBRACK; G.RBRACK; G.LPAREN; G.RPAREN; G.DOT]

let operators_test_case =
  token_test_case "operators/symbol-case"
    ["all operators and symbols", operators_test]

let all_test_cases =
  [lex_claim_test_case;
   lex_int_test_case;
   lex_float_test_case;
   lex_id_test_case;
   lex_string_test_case;
   lex_bool_test_case;
   lex_def_test_case;
   lex_datatype_test_case;
   lex_case_test_case;
   lex_ability_test_case;
   lex_let_test_case;
   lex_fn_test_case;
   lex_mut_test_case;
   lex_end_test_case;
   lex_if_test_case;
   lex_else_test_case;
   lex_then_test_case;
   program_test_case;
   operators_test_case]
