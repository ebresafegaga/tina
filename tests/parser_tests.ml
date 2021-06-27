
open Parser
open Syntax

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
  let pp _formatter (_token : Grammar.token) = () in
  Alcotest.testable pp (=)



let token_test str token error () =  
  Alcotest.check (Alcotest.list token_testable) error
    [token]
    (tokenize str)

let lex_claim_test = token_test "claim" Grammar.CLAIM "claim should result in token claim"

let lex_claim_text_case =
  "claim-case", [Alcotest.test_case "lex claim only" `Quick lex_claim_test]

let all_test_cases =
  [lex_claim_text_case]
