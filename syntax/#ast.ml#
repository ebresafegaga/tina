
open Naming

(* TODO: add locations to pattern *)
type pattern = 
    | PInteger of int 
    | PString of string 
    | PBool of bool
    (* | Pfloat of float  
       | PUnit *)
    | PVariable of VarName.t
    | PRecord of DataName.t * (FieldName.t * pattern) list 
    | PVariant of VarName.t * pattern list
    | PTuple of pattern list 

(* we need a variable type which enacpsulates 
    VarName.t DefName.t ... *)
(* type identifier = V of VarName.t | D of DefName.t | F of FieldName.t *)

(* module Identifier = struct 
    type t = Global of DefName.t | Local of VarName.t  
   end *)

type expression = 
  | LitTodo of Loc.t 
  | LitUnit of Loc.t 
  | LitBool of Loc.t * bool 
  | LitInteger of Loc.t * int 
  | LitFloat of Loc.t * float 
  | LitString of Loc.t * string 

  | Variable of Loc.t * VarName.t
  | If of Loc.t * expression * expression * expression
  | Application of Loc.t * expression * expression list 
  | Let of Loc.t * pattern * expression * expression
  (* LetMut maybe? *)
  | Fn of Loc.t * VarName.t list * expression
  | Annotated of Loc.t * expression * Type.t 
  | Sequence of Loc.t * expression * expression
  | Case of Loc.t * expression * (pattern * expression) list (* tbi *)
  | Record of Loc.t * DataName.t * (FieldName.t * expression) list
  | RecordIndex of Loc.t * expression * FieldName.t
  | Tuple of Loc.t * expression list
  | Variant of Loc.t * DataName.t * expression list
  (* list? tuples? *)
  | Do of Loc.t * VarName.t * expression list
  | Handle of Loc.t * expression * handler_clauses list (* must always have a return clause *)
  | Absurd of string * expression

and handler_clauses =
  | Return of VarName.t * expression
  | Operation of VarName.t * VarName.t list * VarName.t * expression (* ability name, values gotten, cont-var name*)

type toplevel =
  | Claim of Loc.t * VarName.t * Type.t
  | Def of Loc.t * VarName.t * expression
  | VariantDef of Loc.t * DataName.t * (VarName.t * Type.t list) list
  | RecordDef of Loc.t * DataName.t * (FieldName.t * Type.t) list
  | AbilityDef of Loc.t * VarName.t * Type.t list
  | Expression of expression


(* pretty printing facilities for the the ast *)



let pp_list es f = es |> List.map f |> String.concat ", "
  
let rec pp_pattern = function
  | PInteger i -> Int.to_string i
  | PString s -> s
  | PVariable name -> VarName.to_string name
  | PTuple es -> Printf.sprintf "(%s)" (pp_list es pp_pattern)
  | PBool b -> Bool.to_string b
  | PVariant (name, es) -> Printf.sprintf "%s { %s }" (VarName.to_string name) (pp_list es pp_pattern)
  | PRecord (name, es) ->
    Printf.sprintf "%s {%s}" (DataName.to_string name) @@
    pp_list es (fun (name, pattern) -> Printf.sprintf "%s: %s" (FieldName.to_string name) (pp_pattern pattern))

let rec pp_expression = function 
  | LitTodo _loc -> "TODO"
  | LitUnit _loc -> "()"
  | LitBool (_loc, b) -> Bool.to_string b
  | LitInteger (_loc, i) -> Int.to_string i
  | LitFloat (_loc, f) -> Float.to_string f
  | LitString (_loc, s) -> s
  | Variable (_loc, v) -> VarName.to_string v
  | If (_loc, pred, tru, fals) ->
    Printf.sprintf "if %s then %s else %s"
      (pp_expression pred)
      (pp_expression tru)
      (pp_expression fals)
  | Application (_loc, rand, es) ->
    Printf.sprintf "%s (%s)"
      (pp_expression rand)
      (pp_list es pp_expression)
  | Let (_loc, var, value, body) ->
    Printf.sprintf "let %s = %s; %s"
      (pp_pattern var)
      (pp_expression value)
      (pp_expression body)
  | Fn (_loc, names, body) ->
    Printf.sprintf "fn (%s) %s"
      (pp_list names VarName.to_string)
      (pp_expression body)
  | Annotated (_loc, expr, ty) ->
    Printf.sprintf "(the %s %s)"
      (pp_expression expr)
      (Type.pp_ty ty)
  | Sequence (_loc, a, b) ->
    Printf.sprintf "%s; %s;"
      (pp_expression a)
      (pp_expression b)
  | Case (_loc, expr, pes) -> (* pes - pattern, expression S *)
    let f (pat, expr) =
      Printf.sprintf "%s -> %s"
        (pp_pattern pat)
        (pp_expression expr)
    in
    Printf.sprintf "case %s { %s }" 
      (pp_expression expr)
      (pp_list pes f)
  | Tuple (_loc, es) ->
    Printf.sprintf "(%s)" (pp_list es pp_expression)
  | Record (_loc, name, fes) -> (* fes - field, expression S *)
    let f (field, expr) =
      Printf.sprintf "%s: %s"
        (FieldName.to_string field)
        (pp_expression expr)
    in
    Printf.sprintf "%s {%s}"
      (DataName.to_string name)
      (pp_list fes f)
  | RecordIndex (_loc, expr, name) ->
    Printf.sprintf "%s.%s"
      (pp_expression expr)
      (FieldName.to_string name)
  | Variant (_loc, name, []) -> DataName.to_string name
  | Variant (_loc, name, args) ->
    Printf.sprintf "%s (%s)" (DataName.to_string name) (pp_list args pp_expression)
  | Do _ | Handle _ -> ""
  | Absurd (s, e) ->
    Printf.sprintf "absurd (%s, %s)" s (pp_expression e)

let pp_toplevel = function
  | Claim (_loc, name, ty) ->
    Printf.sprintf
      "claim %s %s"
      (VarName.to_string name)
      (Type.pp_ty ty)
  | Def (_loc, name, expr) -> (* TODO: add a special case for fn *)
    Printf.sprintf "def %s = %s"
      (VarName.to_string name)
      (pp_expression expr)
  | Expression expr -> pp_expression expr
  | VariantDef _ | RecordDef _ | AbilityDef _ -> "<def>" (* for now *)


let rec is_value = function 
  | Variable _ 
  | LitUnit _
  | LitInteger _ 
  | LitBool _
  | LitFloat _
  | LitString _ -> true 
  | Annotated (_, e, _) -> is_value e
  | If _ -> true
  | Let _ -> false
  | Fn _ -> true 
  | Application _ -> false
  | Record _ -> true
  | RecordIndex _ -> true
  | Case _ -> true
  | Tuple _ -> true 
  | Sequence _ -> false 
  | LitTodo _ -> true
  | Variant _ -> true
  | Absurd _ -> false (* hack *)
  | Do _ | Handle _ -> false

