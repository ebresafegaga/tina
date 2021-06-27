open Naming

type ty = 
    | TyBool
    | TyString
    | TyNat
    | TyInt
    | TyFloat
    | TyUnit
    | TyArrow of ty list * ty
    | TyRecord of (FieldName.t * ty) list
    | TyTuple of ty list 
    (* Variant type? *)


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
    | Annotated of Loc.t * expression * ty 
    | Sequence of Loc.t * expression * expression
    | Case of Loc.t * expression * (pattern * expression) list (* tbi *)
    | Record of Loc.t * DataName.t * (FieldName.t * expression) list
    | RecordIndex of Loc.t * expression * FieldName.t
    | Tuple of Loc.t * expression list 
    (* | Variant of Loc.t * DataName.t * expression list *)
    (* list? tuples? *)

type toplevel = 
    | Claim of Loc.t * VarName.t * ty
    | Def of Loc.t * VarName.t * expression
    | VariantDef of Loc.t * DataName.t * (VarName.t * ty list) list
    | RecordDef of Loc.t * DataName.t * (FieldName.t * ty) list 
    | Expression of expression

(* pretty printing facilities for the the ast *)

let pp_list es f = es |> List.map f |> String.concat ", "

let rec pp_ty = function
  | TyNat -> "Nat"
  | TyString -> "String"
  | TyInt -> "Int"
  | TyFloat -> "Float"
  | TyBool -> "Bool"
  | TyUnit -> "Unit"
  | TyTuple ts -> Printf.sprintf "(%s)" (pp_list ts pp_ty)
  | TyRecord ts ->
    Printf.sprintf "{%s}" @@
    pp_list ts (fun (n, t) -> Printf.sprintf "claim %s %s" (FieldName.to_string n) (pp_ty t))
  | TyArrow (ts, t) -> Printf.sprintf "(%s) -> %s" (pp_list ts pp_ty) (pp_ty t)
  
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
      (pp_ty ty)
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

let pp_toplevel = function
  | Claim (_loc, name, ty) ->
    Printf.sprintf
      "claim %s %s"
      (VarName.to_string name)
      (pp_ty ty)
  | Def (_loc, name, expr) -> (* TODO: add a special case for fn *)
    Printf.sprintf "def %s = %s"
      (VarName.to_string name)
      (pp_expression expr)
  | Expression expr -> pp_expression expr
  | VariantDef _ | RecordDef _ -> "<def>" (* for now *)
