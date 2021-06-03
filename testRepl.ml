open Syntax
open Parser
open Lexer
open Runtime

let parse lexbuf = 
    match Grammar.toplevel Lexer.read_token lexbuf with 
    | result -> Ok result
    | exception SyntaxError msg -> Error msg
    | exception Grammar.Error -> Error "parse error i guess"

let p () = 
    let file = open_in "b.tina" in
    let l = Lexing.from_channel file in 
    match parse l with 
    | Ok (xs) -> xs |> List.map (fun (Ast.Expression e )-> e |> Eval.eval Eval.empty_env)
    | Error msg -> failwith (Format.sprintf "%s" msg)