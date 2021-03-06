open Syntax
open Naming
open Utility
open Errors
    
module A = DesugarEffect

let rec pat_freevars = function
  | A.PInteger _ | A.PString _
  | A.PBool _ -> []
  | A.PVariable v -> [v]
  | A.PTuple pats ->
    pats
    |> List.map pat_freevars
    |> List.concat
  | A.PVariant (_, pats) ->
    pats
    |> List.map pat_freevars
    |> List.concat
  | A.PRecord (_, pats) ->
    pats
    |> List.map snd
    |> List.map pat_freevars
    |> List.concat

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

  | A.Let (loc, pat, expr, body) ->
    let free = pat_freevars pat in
    let mem = List.mem variable free in
    if mem then
      A.Let (loc, pat, s expr, body) (* lexical scoping! *)
    else
      A.Let (loc, pat, s expr, s body)

  | A.Fn (loc, names, body) ->
    let mem = List.mem variable names in
    if mem then A.Fn (loc, names, body) else A.Fn (loc, names, s body) (* lexical scoping *)
        
  | A.Application (loc, operator, operands) -> A.Application (loc, s operator, List.map s operands)
  | A.Record (loc, name, body) ->
    let body = List.map (fun (fn, e) -> (fn, s e)) body in
    A.Record (loc, name, body)
  | A.RecordIndex (loc, record, field) -> A.RecordIndex (loc, s record, field)

  | A.Case (loc, expr, cases) ->
    let cases =
      cases
      |> List.map (fun (pat, e) ->
          let free = pat_freevars pat in
          let mem = List.mem variable free in
          if mem then (pat, e) else (pat, s e)) (* again, lexical scoping *)
    in
    A.Case (loc, s expr, cases)

  | A.Tuple (loc, exprs) -> A.Tuple (loc, List.map s exprs)
  | A.Variant (loc, name, exprs) -> A.Variant (loc, name, List.map s exprs)
  | A.Sequence (loc, e1, e2) -> A.Sequence (loc, s e1, s e2)
  | A.Absurd (label, e) -> A.Absurd (label, s e)
  (* | A.Plain e -> s e
  | A.Do (loc, name, exprs) -> A.Do (loc, name, List.map s exprs)
  | A.Handle (loc, expr, clauses) ->
    let f = function
      | A.Return (name, body) -> A.Return (name, s body)
      | A.Operation (name, args, kvar, body) -> A.Operation (name, args, kvar, s body)
    in
    let clauses = clauses |> List.map f in
     A.Handle (loc, s expr, clauses) *)

let subst_list = List.fold_right (fun (x, v) e -> subst x v e)

(* let print_env env =
  let e = env |> Env.to_seq |> List.of_seq in
  let env_str = A.pp_list e (fun (n, v) -> Printf.sprintf "%s=%s" (VarName.to_string n) (pp_value v)) in
  print_string "in the enviroment ";
   print_endline env_str *)

exception PatternFailure of string

let rec is_value = function
  | A.Absurd _ 
  | A.Variable _ 
  | A.LitUnit _
  | A.LitInteger _ 
  | A.LitBool _
  | A.LitFloat _
  | A.LitString _ -> true 
  | A.Annotated (_, e, _) -> is_value e
  | A.If _ -> false 
  | A.Let _ -> false
  | A.Fn _ -> true 
  | A.Application _ -> false 
  | A.Record _ -> true
  | A.RecordIndex _ -> false 
  | A.Case _ -> false
  | A.Tuple _ -> true 
  (* | A.Plain e -> is_value e *)
  | A.Sequence _ -> false 
  | A.LitTodo _ -> true
  | A.Variant _ -> true
(* | A.Do _ | A.Handle _ -> failwith "should not be evaluated by me" *)

let rec eval = function
  | A.Variable (_loc, name) ->
    Errors.runtime @@ Printf.sprintf "Unbound Variable %s" @@ VarName.to_string name
  | A.LitUnit loc -> A.LitUnit loc
  | A.LitInteger (loc, i) -> A.LitInteger (loc, i)
  | A.LitBool (loc, b) -> A.LitBool (loc, b)
  | A.LitFloat (loc, f) -> A.LitFloat (loc, f)
  | A.LitString (loc, s) -> A.LitString (loc, s)
  | A.Annotated (_loc, e, _) -> eval e

  | A.If (_loc, A.LitBool (_, b), pt, pf) -> (
      match b with 
      | true -> eval pt
      | false -> eval pf)
  | A.If (_loc, p, _pt, _pf) when is_value p ->
    Errors.runtime @@ Printf.sprintf "expected a bool at an if expression but got %s" @@ A.pp_expression p
  | A.If (loc, p, pt, pf) -> eval @@ A.If (loc, eval p, pt, pf)

  | A.Let (_loc, pat, expr, body) ->
    let sub = pattern_binder pat expr in
    eval (subst_list sub body)
  | A.Fn (loc, names, body) -> A.Fn (loc, names, body)

  | A.Application (_loc, (A.Fn (_, vars, _) as f), args) when List.length args <> List.length vars ->
    let expected, got = List.length vars, List.length args in
    let msg =
      Printf.sprintf "This function %s expected %d argument(s), but got %d"
        (A.pp_expression f) expected got 
    in
    Errors.runtime msg
  | A.Application (_loc, A.Fn (_, vars, body), args) -> eval @@ subst_list (List.combine args vars) body
  | A.Application (loc, Variable (_, v), [A.LitInteger (_, i); A.LitInteger (_, i')])
    when VarName.to_string v = "+" ->
    A.return @@ A.LitInteger (loc, i + i') (* we should really be returning this as a computation *)
  | A.Application (_loc, f, _args) when is_value f -> 
    Errors.runtime @@ Printf.sprintf "this value %s is not a function so it can't be applied" (A.pp_expression f)
  | A.Application (loc, f, args) -> eval @@ A.Application (loc, eval f, args)

  | A.Record (loc, name, body) -> A.Record (loc, name, body)

  | A.RecordIndex (_loc, A.Record (_, _name, fields), field) -> (
      match List.assoc_opt field fields with 
      | Some value -> value 
      | None ->
        Errors.runtime @@
        Printf.sprintf "That field name %s is not defined on the record"
          (FieldName.to_string field))
  | A.RecordIndex (_loc, record, _field) when is_value record -> Errors.runtime "Expected a record at an index expression"
  | A.RecordIndex (loc, record, field) -> eval @@  A.RecordIndex (loc, eval record, field)


  | A.Variant (loc, name, args) when List.for_all is_value args -> A.Variant (loc, name, args)
  | A.Variant (loc, name, args) -> A.Variant (loc, name, List.map eval args)

  | A.Case (_loc, expr, cases) -> 

    let rec eval_cases msg = function
      | [] -> Errors.runtime msg
      | x :: xs ->
        let p, e = x in
        match pattern_binder p expr with 
        | sub -> eval (subst_list sub e)
        | exception PatternFailure msg' ->
          let msg = Printf.sprintf "%s | %s" msg msg' in
          eval_cases msg xs
    in 
    eval_cases "" cases

  | A.Tuple (loc, exprs) ->  A.Tuple (loc, exprs)
  (* | A.Plain e -> eval e *)
  | A.Sequence (_loc, _e1, _e2) -> Errors.runtime "Sequence expressions not yet implemented"
  | A.Absurd (s, e) ->
    let msg = Printf.sprintf "%s, %s" s (A.pp_expression e) in
    Errors.runtime msg
  | A.LitTodo _loc -> Errors.runtime "Not yet supported"
(* | A.Do _ | A.Handle _ -> Errors.runtime "effects are not supported by this evaluator" *)

and pattern_binder pattern value = 
  let length_check l1 l2 =
    let len1, len2 = List.length l1, List.length l2 in
    let msg = Printf.sprintf
        "can't match because the length of a tuple or variant arguments aren't equal. want: %d, got: %d. pattern: %s, expression %s"
        len1 len2 (A.pp_list l1 A.pp_pattern) (A.pp_list l2 A.pp_expression)
    in
    if len1 <> len2
    then raise @@ PatternFailure msg
  in
  match pattern, value with
  | A.PVariable name, value ->  [value, name]
  | A.PInteger i, A.LitInteger (_, i') when i = i' -> []
  | A.PString s, A.LitString (_, s') when s = s' -> []
  | A.PBool b, A.LitBool (_, b') when b = b' -> []
  | A.PRecord (name, body), A.Record (_, name', body') when name = name' -> 
    let extender (n, p) env = 
      match List.assoc_opt n body' with 
      | Some v -> pattern_binder p v @ env
      | None -> failwith "Field does not exist" (* TODO: use Result monad *)
    in 
    List.fold_right extender body []
  | A.PVariant (name, patterns), A.Variant (_, name', values)
     when VarName.to_string name = DataName.to_string name' -> 
    length_check patterns values;
    List.fold_right2 (fun p v env -> pattern_binder p v @ env) patterns values []
  | A.PTuple patterns, A.Tuple (_, values) -> 
    length_check patterns values;
    List.fold_right2 (fun p v env -> pattern_binder p v @ env) patterns values []
  | _pattern, expression when is_value expression ->
    let msg = Printf.sprintf "The pattern %s doens't match the expression %s"
        (A.pp_pattern pattern) (A.pp_expression expression)
    in
    raise @@ PatternFailure msg
  | pattern, expression -> pattern_binder pattern (eval expression)

let rec subst_toplevel names = function
  | A.Def (loc, name, body) :: rest ->
    let x = subst_list names body in
    let names' = (x, name) :: names in
    A.Def (loc, name, x) :: subst_toplevel names' rest 
  | A.Expression e :: rest ->
    A.Expression (subst_list names e) :: subst_toplevel names rest

  (* trivial cases *)
  | [] -> []
  | A.Claim _ as c :: rest -> c :: subst_toplevel names rest
  | A.RecordDef _ as rd :: rest -> rd :: subst_toplevel names rest
  | A.AbilityDef _ as ad :: rest -> ad :: subst_toplevel names rest
  | A.VariantDef _ as vd  :: rest -> vd :: subst_toplevel names rest

let rec process_toplevel = function
  | [] -> []
  | A.Claim (_loc, _, _) :: rest -> process_toplevel rest 
  | A.Def (_loc, name, body) :: rest ->
    let env = [eval body, name] in
    let tops = subst_toplevel env rest in
    process_toplevel tops 
  | A.Expression e :: rest -> 
    A.pp_expression (eval e) :: process_toplevel rest  
  | A.RecordDef (_loc, _, _) :: rest -> process_toplevel  rest
  | A.AbilityDef _ :: rest -> process_toplevel rest  (* do nothing for now *)
  | A.VariantDef (_loc, _name, _body) :: rest -> process_toplevel rest
(* | A.VariantDef (_loc, _name, body) :: rest ->
   let variant_extend (name, l) env =
    match l with 
    | [] -> Env.add name (V.VVariant (name, [])) env
    | _ :: _ -> 
      let clo = guard_values_by_len (List.length l) (fun values -> V.VVariant (name, values)) in
      Env.add name (V.VClosure clo) env
   in
   let env = List.fold_right variant_extend body env in
   process_toplevel env rest  *)

