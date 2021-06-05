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
    let file = Sys.argv.(1) in
    let file = open_in file in
    let l = Lexing.from_channel file in 
    match parse l with 
    | Ok (xs) ->
        xs 
        |> Eval.process_toplevel
        |> String.concat "\n"
        |> Printf.sprintf "%s"
    | Error msg -> failwith (Format.sprintf "%s" msg)