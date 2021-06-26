open Syntax
open Parser
open Lexer
open Runtime

let parse lexbuf =
    match Grammar.toplevel Lexer.read_token lexbuf with 
    | result -> Ok result
    | exception SyntaxError msg -> Error msg
    | exception Grammar.Error -> Error "parse error i guess"

let () = 
  let file = Sys.argv.(1) in
  let file = open_in file in
  Lexing.from_channel file
  |> ParserEntry.parse
  |> Eval.process_toplevel 
  |> String.concat "\n"
  |> Printf.printf "%s\n"
