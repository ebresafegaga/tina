
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
    | A.PVariant (name, body), V.VVariant (name', body') when name = name' -> 
        if List.length body <> List.length body' 
            then failwith "Invalid number of arguments for pattern match" ; (* TODO use Result Monad *)
        List.fold_right2 (fun p v env -> pattern_binder p v env) body body' env
    | A.PTuple (patterns), V.VTuple (values) -> 
        if List.length patterns <> List.length values 
            then failwith "Invalid number of arguments for pattern match" ; (* TODO use Result Monad *)
        List.fold_right2 pattern_binder patterns values env
    | _ -> raise PatternFailure

let rec eval env expr = 
    match expr with
    | A.Variable (_loc, name) -> 
        (match lookup name env with 
         | Some x -> Ok x 
         | None -> Error (Printf.sprintf "Unbound Variable %s" (VarName.to_string name)))

    | A.LitUnit _loc -> Ok V.VUnit 
    | A.LitInteger (_loc, i) -> Ok (V.VInteger i)
    | A.LitBool (_loc, b) -> Ok (V.VBool b)
    | A.LitFloat (_loc, f) -> Ok (V.VFloat f)
    | A.LitString (_loc, s) -> Ok (V.VString s)
    | A.Annotated (_loc, e, _) -> eval env e

    | A.If (_loc, e, pt, pf) -> 
        let open Result in 
        let* e = eval env e in 
        (match e with 
         | V.VBool true -> eval env pt
         | V.VBool false -> eval env pf
         | _ -> Error "expected a bool type at an if expression ")

    | A.Let (_loc, name, expr, body) ->
        let open Result in 
        let* value = eval env expr in
        let env = Env.add name value env in
        eval env body

    | A.Fn (_loc, names, body) -> 
        let clo values = 
            (* This check might not be valid if I change 
               the representation of closures to use frames. *)
            if List.length values <> List.length names then 
                Error (Printf.sprintf "Invalid Number of arguments" )
            else
                let env = List.fold_right2 Env.add names values env in
                eval env body
        in
        Ok (V.VClosure clo)

    | A.Application (_loc, operator, operands) ->
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

    | A.Record (_loc, name, body) -> 
        let open Result in
        let names, exprs = List.split body in
        let* values = exprs |> List.map (eval env) |> Result.sequenceA in
        let body = List.combine names values in 
        Ok (V.VRecord (name, body))

    | A.RecordIndex (_loc, record, field) -> 
        let open Result in
        let* record = eval env record in
        (match record with 
        | V.VRecord (_, fields) -> 
           (match List.assoc_opt field fields with 
            | Some value -> Ok value 
            | None -> Error "That field is not defied on the record")
        | _ -> Error "Not a record")

    | A.Case (_loc, expr, cases) -> 
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
    | A.Tuple (_loc, exprs) -> 
        let open Result in
        let* result = exprs |> List.map (eval env) |> Result.sequenceA in
        Ok (V.VTuple result)

    | A.Sequence (loc, e1, e2) -> 
        Error "Sequence expressions not yet implemented"
    | A.LitTodo loc -> Error "Not yet supported"

let guard_values_by_len n f values = 
    let arg_len = List.length values in 
    if n <> arg_len then 
        Error "Invalid number of arguments"
    else 
        Ok (f values)  
    
let is_fn = function A.Fn _ -> true | _ -> false 

let rec process_toplevel env = function  
    | [] -> []
    | A.Claim (loc, _, _) :: rest -> process_toplevel env rest 
    | A.Def (loc, name, body) :: rest when is_fn body ->
        let env' = ref env in 
        let body_value = 
            V.VClosure (fun values -> 
                match eval !env' body with 
                | Ok (V.VClosure f) -> f values
                | Ok _ -> Error "This value is not a function so it can't be applied"
                | Error s -> Error s) 
        in 
        env' := Env.add name body_value !env';
        process_toplevel !env' rest
    | A.Def (loc, name, body) :: rest -> 
        let body_value = eval env body in 
        (match body_value with 
        | Ok value -> 
            let env = Env.add name value env in 
            process_toplevel env rest
        | Error s -> Printf.sprintf "Error: %s" s :: process_toplevel env rest)
    | A.Expression e :: rest -> 
        (match eval env e with 
        | Ok value -> (V.pp_value value) :: process_toplevel env rest
        | Error s -> Printf.sprintf "Error: %s" s :: process_toplevel env rest)
    | A.RecordDef (loc, _, _) :: rest -> process_toplevel env rest 
    | A.VariantDef (loc, name, body) :: rest ->
        let variant_extend (name, l) env =
            match l with 
            | [] -> Env.add name (V.VVariant (name, [])) env
            | _ :: _ -> 
                let clo = guard_values_by_len (List.length l) (fun values -> V.VVariant (name, values)) in
                Env.add name (V.VClosure clo) env
        in
        let env = List.fold_right variant_extend body env in
        process_toplevel env rest

let operator op = 
    let clo = function  
        | [V.VInteger x; V.VInteger y] -> Ok (V.VInteger (op x y))
        | [_; _] -> Error "Expected integers"
        | _ -> Error "Invalid Number of arguemnts"
    in
    V.VClosure clo

let addition = operator (+)
let minus = operator (-)
let times = operator ( * )
let div = operator (/)
    
let global_env = 
    [ "+", addition; 
      "-", minus;
      "*", times;
      "/", div; ]
    |> List.map (fun (name, value) -> (VarName.of_string name, value))
    |> List.to_seq
    |> Env.of_seq

let process_toplevel = process_toplevel global_env