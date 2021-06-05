
open Syntax
open Naming 
open Utility.Util

module A = Ast
module V = Value

module Env : Map.S with type key := VarName.t = struct 
    include Map.Make (VarName)
end
let lookup = Env.find_opt

let empty_env =  Env.empty

exception PatternFailure 

let rec pattern_binder pattern value env = 
    match pattern, value with 
    | A.PVariable name, value -> Env.add name value env
    | A.PInteger i, V.VInteger i' when i = i' -> env
    | A.PString s, V.VString s' when s = s' -> env
    | A.PBool b, V.VBool b' when b = b' -> env
    | A.PRecord (name, body), V.VRecord (name', body') when name = name' -> 
        let extender (n, p) env = 
            match List.assoc_opt n body' with 
            | Some v -> pattern_binder p v env
            | None -> failwith "Field does not exist" (* TODO: use Result monad *)
        in 
        List.fold_right extender body env
    | _ -> raise PatternFailure

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

    | A.If (loc, e, pt, pf) -> 
        let open Result in 
        let* e = eval env e in 
        (match e with 
         | V.VBool true -> eval env pt
         | V.VBool false -> eval env pf
         | _ -> Error "expected a bool type at an if expression ")

    | A.Let (loc, name, expr, body) ->
        let open Result in 
        let* value = eval env expr in
        let env = Env.add name value env in
        eval env body

    | A.Fn (loc, names, body) -> 
        let clo values = 
            let env = List.fold_right2 Env.add names values env in
            eval env body
        in
        Ok (V.VClosure clo)

    | A.Application (loc, operator, operands) -> 
        (* TODO check that number of arguments are equal *)
        let open Result in
        let* operands = 
            operands 
            |> List.map (eval env)  
            |> Result.sequenceA 
        in
        (match eval env operator with 
         | Ok (V.VClosure f) -> f operands
         | Ok _ -> Error "This expression is not a function, so it cannot be invoked"
         | Error s -> Error s)

    | A.Record (loc, name, body) -> 
        let open Result in
        let names, exprs = List.split body in
        let* values = exprs |> List.map (eval env) |> Result.sequenceA in
        let body = List.combine names values in 
        Ok (V.VRecord (name, body))

    | A.RecordIndex (loc, record, field) -> 
        let open Result in
        let* record = eval env record in
        (match record with 
        | V.VRecord (_, fields) -> 
           (match List.assoc_opt field fields with 
            | Some value -> Ok value 
            | None -> Error "That field is not defied on the record")
        | _ -> Error "Not a record")

    | A.Case (loc, expr, cases) -> 
        let open Result in
        let* value = eval env expr in
        let rec eval_cases = function 
            | [] -> Error "Pattern match failure"
            | x :: xs -> 
                let p, e = x in
                match pattern_binder p value env with 
                | env -> eval env e
                | exception PatternFailure -> eval_cases xs
        in 
        eval_cases cases 
    | A.Sequence (loc, e1, e2) -> 
        Error ""
    | A.LitTodo loc -> Error "Not yet supported" 


let rec process_toplevel env = function  
    | [] -> []
    | A.Claim (loc, _, _) :: rest -> process_toplevel env rest 
    | A.Def (loc, name, body) :: rest -> 
        let body_value = eval env body in 
        (match body_value with 
        | Ok value -> 
            let env = Env.add name value env in 
            process_toplevel env rest
        | Error s -> Error s :: process_toplevel env rest)
    | A.Expression e :: rest -> 
        (match eval env e with 
        | Ok value -> Ok (V.pp_value value) :: process_toplevel env rest
        | Error s -> Error s :: process_toplevel env rest)
    | A.RecordDef (loc, _, _) :: rest -> process_toplevel env rest 