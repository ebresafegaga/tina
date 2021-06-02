open Naming

type ty = 
    | TyString
    | TyNat
    | TyInt
    | TyFloat
    | TyUnit
    | TyArrow of ty list * ty
    | TyRecord of (FieldName.t * ty) list
    (* Variant type? *)

type constexpr = 
    | CBool of bool 
    (* | CNat of int  *)
    | CInt of int
    | CFloat of float 
    | CString of string 

(* tbi *)
type pattern = 
    | PVariable of VarName.t
    | PRecord of DataName.t * (FieldName.t * pattern) list 
    (* | PVariant of DataName.t * pattern list  *)

(* we need a variable type which enacpsulates 
    VarName.t DefName.t ... *)
type identifier = V of VarName.t | D of DefName.t | F of FieldName.t

type expression = 
    | Unit of Loc.t 
    | Variable of Loc.t * VarName.t
    | Constant of Loc.t * constexpr
    | If of Loc.t * expression * expression * expression
    | Application of Loc.t * expression * expression list 
    | Let of Loc.t * VarName.t * expression * expression
    (* LetMut maybe? *)
    | Fn of Loc.t * VarName.t list * expression
    | Annotated of Loc.t * expression * ty 
    | Sequence of Loc.t * expression * expression
    | Match of Loc.t * expression * (pattern * expression) list (* tbi *)
    | Record of Loc.t * DataName.t * (FieldName.t * expression) list
    | RecordIndex of Loc.t * expression * FieldName.t
    (* | Variant of Loc.t * DataName.t * expression list *)
    | TODO of Loc.t 
    (* list? tuples? *)

type toplevel = 
    | Claim of Loc.t * DefName.t * ty
    | Def of Loc.t * DefName.t * expression
    (* | VariantDef of Loc.t * DataName.t * (CtorName.t * ty list) list *)
    | RecordDef of Loc.t * DataName.t * (FieldName.t * ty) list 
    | Expression of expression



(*

    

    data People = gaga | Someone (String, String) | ... 

    a = Name { A: 2, B: 45 }

    a.A
  
    match a with 
    | Name { a: <pat>, b: <pat> } -> 
    
    b = Ogaga 
    c =
*)