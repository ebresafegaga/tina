
open Syntax
open Naming 
open Utility.Util

module A = Ast
module V = Value

module Env : Map.S with type key := VarName.t = struct 
    include Map.Make (VarName)
end

let lookup = Env.find_opt

let l = List.fold_right2
let g = Env.add

let rec eval env expr = 
    match expr with
    | A.Variable (loc, name) -> 
        (match lookup name env with 
         | Some x -> Ok x 
         | None -> Error (Printf.sprintf "Unbound Variable %s" (VarName.to_string name)))

    | A.LitUnit loc -> Ok V.VUnit 
    | A.LitInteger (loc, i) -> Ok (V.VInteger i)
    | A.LitBool (loc, b) -> Ok (V.VBool b)
    | A.LitFloat (loc, f) -> Ok (V.VFloat f)
    | A.LitString (loc, s) -> Ok (V.VString s)
    | A.Annotated (loc, e, _) -> eval env e

    | A.Fn (loc, names, body) -> 
        Ok (V.VClosure (fun values -> 
            let env' = List.fold_right2 Env.add names values env in
            eval env' body))

    | A.Application (loc, operator, operands) -> 
        let open Result in
        let* operands' = 
            operands 
            |> List.map (eval env)  
            |> Result.sequenceA 
        in
        (match eval env operator with 
         | Ok (V.VClosure f) -> f operands' 
         | Ok _ -> Error "This expression is not a function, so it cannot be invoked"
         | Error s -> Error s)

    | A.Case (loc, expr, cases) -> Error ""

    | A.Sequence (loc, e1, e2) -> 
        Error ""
    | A.LitTodo loc -> Error "Not yet supported" 
    | _ -> Error ""
