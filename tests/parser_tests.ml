
open Parser


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
    
let claim = "claim"

let lex_claim_test () =
  Alcotest.check (Alcotest.list token_testable) "claim should result in token claim"
    [Grammar.CLAIM]
    (tokenize claim)

let lex_claim_text_case =
  "claim-case", [Alcotest.test_case "lex claim only" `Quick lex_claim_test]

let all_test_cases =
  [lex_claim_text_case]
