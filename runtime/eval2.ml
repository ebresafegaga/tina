open Syntax
open Naming
open Utility

module A = Ast

(* Enviroment mangement *)
module type S = sig 
    include Map.S
    val lookup : key -> 'a t -> 'a option
end

module Env : S with type key := VarName.t = struct 
    include Map.Make (VarName)
    let lookup = find_opt
end
  
type value =
  | VUnit
  | VInteger of int
  | VString of string
  | VFloat of float
  | VBool of bool
  | VTuple of value list
  | VClosure of env * VarName.t list * Ast.expression
  | VRecord of DataName.t * (FieldName.t * value) list 
  | VVariant of VarName.t * value list

and env = value Env.t

let rec pp_value v = 
    let pp_value_list values sep = values |> List.map pp_value |> String.concat sep in
    match v with 
    | VUnit -> "(void)"
    | VInteger i -> Int.to_string i
    | VString s -> Printf.sprintf "%s%s%s" {|"|} s {|"|} 
    | VFloat f -> Float.to_string f 
    | VBool b -> Bool.to_string b
    | VClosure _ -> "<fun>" (* we can't inspect the body of the closure if we like *)
    | VRecord (name, fields) ->
        let fields_pp = 
            fields  
            |> List.map (fun (name, value) -> Printf.sprintf " %s: %s" (FieldName.to_string name) (pp_value value))
            |> String.concat ","
        in
        Printf.sprintf "%s {%s }" (DataName.to_string name) fields_pp 

    | VVariant (name, []) -> VarName.to_string name
    | VVariant (name, values) -> Printf.sprintf "%s (%s)" (VarName.to_string name) (pp_value_list values ", ")
    | VTuple (values) -> Printf.sprintf "(%s)" (pp_value_list values ", ")

(* Pattern matching handling *)
exception PatternFailure

let rec pattern_binder pattern value env = 
  let length_check l1 l2 =  if List.length l1 <> List.length l2 then raise PatternFailure in
  match pattern, value with 
  | A.PVariable name, value -> Env.add name value env
  | A.PInteger i, VInteger i' when i = i' -> env
  | A.PString s, VString s' when s = s' -> env
  | A.PBool b, VBool b' when b = b' -> env
  | A.PRecord (name, body), VRecord (name', body') when name = name' -> 
    let extender (n, p) env = 
      match List.assoc_opt n body' with 
      | Some v -> pattern_binder p v env
      | None -> failwith "Field does not exist" (* TODO: use Result monad *)
    in 
    List.fold_right extender body env
  | A.PVariant (name, body), VVariant (name', body') when name = name' -> 
    length_check body body';
    List.fold_right2 (fun p v env -> pattern_binder p v env) body body' env
  | A.PTuple patterns, VTuple values -> 
    length_check patterns values;
    List.fold_right2 pattern_binder patterns values env
  | _ -> raise PatternFailure

  (* expression[value/variable]*)
let rec subst value variable e =
  let s = subst value variable in 
  match e with
  | A.Variable (_loc, name) when name = variable -> value
  | A.Variable _ | A.LitUnit _ 
  | A.LitInteger _ | A.LitBool _ | A.LitFloat _
  | A.LitString _ | A.LitTodo _ -> e
  | A.Annotated (loc, e, ty) -> A.Annotated (loc, s e, ty)
  | A.If (loc, p, pt, pf) -> A.If (loc, s p, s pt, s pf)
  | A.Let (loc, pat, expr, body) -> A.Let (loc, pat, s expr, s body)
  | A.Fn (loc, names, body) -> A.Fn (loc, names, s body)
  | A.Application (loc, operator, operands) -> A.Application (loc, s operator, List.map s operands)
  | A.Record (loc, name, body) ->
    let body = List.map (fun (fn, e) -> (fn, s e)) body in
    A.Record (loc, name, body)
  | A.RecordIndex (loc, record, field) -> A.RecordIndex (loc, s record, field)
  | A.Case (loc, expr, cases) ->
    let cases = cases |> List.map (fun (p, e) -> (p, s e)) in
    A.Case (loc, s expr, cases)
  | A.Tuple (loc, exprs) -> A.Tuple (loc, List.map s exprs)
  | A.Plain e -> s e
  | A.Sequence (loc, e1, e2) -> A.Sequence (loc, s e1, s e2)
  | A.Do (loc, name, exprs) -> A.Do (loc, name, List.map s exprs)
  | A.Handle (loc, expr, clauses) ->
    let f = function
      | A.Return (name, body) -> A.Return (name, s body)
      | A.Operation (name, args, kvar, body) -> A.Operation (name, args, kvar, s body)
    in
    let clauses = clauses |> List.map f in
    A.Handle (loc, s expr, clauses)

let subst_list subs expr = List.fold_right (fun (x, v) e -> subst x v e) subs expr

let rec eval env = function
  | A.Variable (_loc, name) -> (
      match Env.lookup name env with 
      | Some x -> Ok x 
      | None -> Error (Printf.sprintf "Unbound Variable %s" (VarName.to_string name)))
  | A.LitUnit _loc -> Ok VUnit 
  | A.LitInteger (_loc, i) -> Ok (VInteger i)
  | A.LitBool (_loc, b) -> Ok (VBool b)
  | A.LitFloat (_loc, f) -> Ok (VFloat f)
  | A.LitString (_loc, s) -> Ok (VString s)
  | A.Annotated (_loc, e, _) -> eval env e
  | A.If (_loc, e', pt, pf) -> 
    let open Result in 
    let* e = eval env e' in 
    (match e with 
     | VBool true -> eval env pt
     | VBool false -> eval env pf
     | _v -> Error "expected a bool type at an if expression" )
  | A.Let (_loc, pat, expr, body) ->
    let open Result in 
    let* value = eval env expr in
    let env = pattern_binder pat value env in
    eval env body
  | A.Fn (_loc, names, body) -> Ok (VClosure (env, names, body))
  | A.Application (_loc, operator, operands) -> apply env operator operands
  | A.Record (_loc, name, body) -> 
    let open Result in
    let names, exprs = List.split body in
    let* values = exprs |> List.map (eval env) |> Result.sequenceA in
    let body = List.combine names values in
    Ok (VRecord (name, body))
  | A.RecordIndex (_loc, record, field) -> (
      let open Result in
      let* record = eval env record in
      match record with 
      | VRecord (_, fields) -> (
          match List.assoc_opt field fields with 
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
    Ok (VTuple result)
  | A.Plain e -> eval env e
  | A.Sequence (_loc, _e1, _e2) -> Error "Sequence expressions not yet implemented"
  | A.LitTodo _loc -> Error "Not yet supported"
  | A.Do _ | A.Handle _ -> failwith "should not be evaluated by me"

and apply env operator operands =
  (* is this lazy applicaton ?? *)
  match eval env operator with 
  | Ok (VClosure (env, vars, body)) ->
    print_endline "got here";
    (* ensure length operands = length vars *)
    let sub = List.combine operands vars in
    eval env (subst_list sub body)
  | Ok _ -> Error (Printf.sprintf "This expression is not a function, so it can't be applied")
  | Error s -> Error s

let is_fn = function A.Fn _ -> true | _ -> false
  
let rec process_toplevel env = function
  | [] -> []
  | A.Claim (_loc, _, _) :: rest -> process_toplevel env rest 
  | A.Def (_loc, name, body) :: rest ->
    (match eval env body with 
     | Ok (VClosure (env_me, names, body) as f) ->
       print_endline "evaluating closure";
       let _env_me = Env.add name f env_me in
       let clo = VClosure (env_me, names, body) in
       let env_global = Env.add name clo env in
       process_toplevel env_global rest
     | Ok value ->
       let env_global = Env.add name value env in
       process_toplevel env_global rest
     | Error s -> Printf.sprintf "Error: %s" s :: process_toplevel env rest)
  | A.Expression e :: rest -> 
    (match eval env e with 
     | Ok value -> (pp_value value) :: process_toplevel env rest
     | Error s -> Printf.sprintf "Error: %s" s :: process_toplevel env rest)
  | A.RecordDef (_loc, _, _) :: rest -> process_toplevel env rest
  | A.AbilityDef _ :: rest -> process_toplevel env rest  (* do nothing for now *)
  | A.VariantDef (_loc, _name, _body) :: _rest -> failwith "variants not yet implemented"
(* | A.VariantDef (_loc, _name, body) :: rest ->
   let variant_extend (name, l) env =
    match l with 
    | [] -> Env.add name (V.VVariant (name, [])) env
    | _ :: _ -> 
      let clo = guard_values_by_len (List.length l) (fun values -> V.VVariant (name, values)) in
      Env.add name (V.VClosure clo) env
   in
   let env = List.fold_right variant_extend body env in
   process_toplevel env rest *)

let process_toplevel = process_toplevel Env.empty

