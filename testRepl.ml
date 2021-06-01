open Syntax
open Parser
open Lexer

let parse lexbuf = 
    match Grammar.toplevel Lexer.read_token lexbuf with 
    | result -> Ok result
    | exception SyntaxError msg -> Error msg
    | exception Grammar.Error -> Error "parse error i guess"

let p () = 
    let file = open_in "b.tina" in
    let l = Lexing.from_channel file in 
    match parse l with 
    | Ok x -> x
    | Error msg -> []