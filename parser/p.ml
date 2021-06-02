
open Syntax
open Lexer

let parse lexbuf = 
    match Grammar.toplevel Lexer.read_token lexbuf with 
    | result -> Ok result
    | exception SyntaxError msg -> Error msg
    | exception Grammar.Error -> Error "parse error i guess"

let a = Ast.Fn (Loc.dummy, [], Ast.LitTodo Loc.dummy)

(*
    claim sum (claim x Nat, Eq (x, 10) -> Nat)
    def sum (x, y) = x + y 

    fn (claim x Nat, claim y Nat) x + y 

    claim a Nat 
    def a = 34

    claim b Int
    def b = 45
*)

type abc = 
    { first : int; 
      mutable second : string }

let a = { first = 34; second = "" }

let () =
    a.second <- "de";
    ()