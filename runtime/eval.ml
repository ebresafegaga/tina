
open Syntax

module A = Ast
module V = Value

let eval env expr = 
    match expr with
    | A.LitUnit loc -> Ok V.VUnit 
    | A.LitInteger (loc, i) -> Ok (V.VInteger i)
    | A.LitTodo loc -> Error "Not yet supported" 
    | A.Case (_, _, _) -> Error ""