open Naming

type ty = 
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
    | Let of Loc.t * VarName.t * expression * expression
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

(*  

    def rec just (c ...) = ...

    datatype People = gaga | Someone (String, String) | ... 

    a = Name { A: 2, B: 45 }

    a.A
  
    match a with 
    | Name { a: <pat>, b: <pat> } -> 
    
    b = Ogaga 
    c =
*)