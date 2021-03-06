
open Syntax
open Naming 
open Utility

module A = Ast
module V = Value

(* Enviroment mangement *)
module type S = sig 
    include Map.S
    val lookup : key -> 'a t -> 'a option
end

module Env : S with type key := VarName.t = struct 
    include Map.Make (VarName)
    let lookup = find_opt
end

(* Pattern matching handling *)
exception PatternFailure of string

let rec pattern_binder pattern value env = 
  let length_check l1 l2 =
    let len1, len2 = List.length l1, List.length l2 in
    let msg = Printf.sprintf
    "can't match because the length of a tuple or variant arguments aren't equal. want: %d, got: %d. pattern: %s, expression %s"
        len1 len2 (A.pp_list l1 A.pp_pattern) (A.pp_list l2 V.pp_value)
    in
    if len1 <> len2
    then raise @@ PatternFailure msg
  in
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
    length_check body body';
    List.fold_right2 (fun p v env -> pattern_binder p v env) body body' env
  | A.PTuple patterns, V.VTuple values -> 
    length_check patterns values;
    List.fold_right2 pattern_binder patterns values env
  | pattern, value ->
    let msg = Printf.sprintf "The value %s doens't match the pattern %s"
        (A.pp_pattern pattern) (V.pp_value value)
    in
    raise @@ PatternFailure msg

let rec eval env = function
  | A.Absurd (s, _e) -> Error s
  | A.Variant (_loc, name, args) ->
    let name =  name |> DataName.to_string |> VarName.of_string in 
    let open Result in
    let* args = 
      args
      |> List.map (eval env)  
      |> Result.sequenceA 
    in
    Ok (V.VVariant (name, args))
  | A.Variable (_loc, name) -> (
      match Env.lookup name env with 
      | Some x -> Ok x 
      | None -> Error (Printf.sprintf "Unbound Variable %s" (VarName.to_string name)))
  | A.LitUnit _loc -> Ok V.VUnit 
  | A.LitInteger (_loc, i) -> Ok (V.VInteger i)
  | A.LitBool (_loc, b) -> Ok (V.VBool b)
  | A.LitFloat (_loc, f) -> Ok (V.VFloat f)
  | A.LitString (_loc, s) -> Ok (V.VString s)
  | A.Annotated (_loc, e, _) -> eval env e
  | A.If (_loc, e', pt, pf) -> 
    let open Result in 
    let* e = eval env e' in 
    (match e with 
     | V.VBool true -> eval env pt
     | V.VBool false -> eval env pf
     | v ->
       Error
         (Printf.sprintf
            "expected a bool type at an if expression, but got expression %s. Also note true-case: %s, false-case %s"
            (V.pp_value v) (A.pp_expression pt) (A.pp_expression pf)))
  | A.Let (_loc, pat, expr, body) ->
    let open Result in 
    let* value = eval env expr in
    let env = pattern_binder pat value env in
    eval env body
  | A.Fn (_loc, names, body) as f -> 
    let clo values =
      let expected, got = List.length names, List.length values in
      (* This check might not be valid if I change 
         the representation of closures to use frames. *)
      if expected <> got then
        Error (Printf.sprintf "function %s: expected %d arguments, but got %d. the arguments are %s"
                 (A.pp_expression f) expected got (A.pp_list values V.pp_value))
      else
        let env = List.fold_right2 Env.add names values env in
        eval env body
    in
    Ok (V.VClosure clo)
  | A.Application (_loc, operator, operands') -> (
      let open Result in
      let* operands = 
        operands' 
        |> List.map (eval env)  
        |> Result.sequenceA 
      in
      match eval env operator with 
      | Ok (V.VClosure f) -> f operands
      | Ok _ ->
        Error
          (Printf.sprintf "This expression %s, is not a function, so it cannot be applied to %s"
             (A.pp_expression operator) (A.pp_list operands' A.pp_expression))
      | Error s -> Error s)
  | A.Record (_loc, name, body) -> 
    let open Result in
    let names, exprs = List.split body in
    let* values = exprs |> List.map (eval env) |> Result.sequenceA in
    let body = List.combine names values in
    Ok (V.VRecord (name, body))
  | A.RecordIndex (_loc, record, field) -> (
      let open Result in
      let* record = eval env record in
      match record with 
      | V.VRecord (_, fields) -> (
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
        | exception PatternFailure _ -> eval_cases xs
    in 
    eval_cases cases 
  | A.Tuple (_loc, exprs) -> 
    let open Result in
    let* result = exprs |> List.map (eval env) |> Result.sequenceA in
    Ok (V.VTuple result)
  | A.Sequence (_loc, _e1, _e2) -> Error "Sequence expressions not yet implemented"
  | A.LitTodo _loc -> Error "Not yet supported"
(* | A.Do _ | A.Handle _ -> failwith "should not be evaluated by me" *)

let guard_values_by_len n f values = 
    let arg_len = List.length values in 
    if n <> arg_len then 
        Error "Invalid number of arguments"
    else 
        Ok (f values)  
    
let is_fn = function A.Fn _ -> true | _ -> false 

let rec process_toplevel env = function
  | [] -> []
  | A.Claim (_loc, _, _) :: rest -> process_toplevel env rest 
  | A.Def (_loc, name, body) :: rest when is_fn body ->
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
  | A.Def (_loc, name, body) :: rest -> 
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
  | A.RecordDef (_loc, _, _) :: rest -> process_toplevel env rest
  | A.AbilityDef _ :: rest -> process_toplevel env rest  (* do nothing for now *)
  | A.VariantDef (_loc, _name, body) :: rest ->
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
        | [a; b] -> Error (Printf.sprintf "dont know how to use op on %s and %s" (V.pp_value a) (V.pp_value b))
        | _ -> Error "Invalid Number of arguemnts"
    in
    V.VClosure clo

let generic_plus = 
  let clo = function
    | [V.VInteger x; V.VInteger y] -> Ok (V.VInteger (x + y))
    | [V.VInteger x; V.VString y] ->
      Ok (V.VString (Printf.sprintf "%d%s" x y))
    | [V.VString y; V.VInteger x] ->
      Ok (V.VString (Printf.sprintf "%s%d" y x ))
    | [V.VString y; V.VString x] ->
      Ok (V.VString (Printf.sprintf "%s%s" y x ))
    | [a; b] -> Error (Printf.sprintf "dont know how to add %s and %s" (V.pp_value a) (V.pp_value b))
    | _ -> Error "Invalid Number of arguemnts"
  in
  V.VClosure clo

let addition = operator (+)
let minus = operator (-)
let times = operator ( * )
let div = operator (/)
    
let global_env = 
  [ "+", generic_plus; 
    "-", minus;
    "*", times;
    "/", div; ]
  |> List.map (fun (name, value) -> (VarName.of_string name, value))
  |> List.to_seq
  |> Env.of_seq

let process_toplevel = process_toplevel global_env


