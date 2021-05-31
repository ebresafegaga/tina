open Utils
open Naming

type ty = 
    | TyString 
    | TyNat 
    | TyInt
    | TyFloat 
    | TyArrow of ty list * ty 
    (* Record type *)

type const = 
    | Bool of bool 
    | Nat of int 
    | Int of int
    | Float of float 
    | String of string 

type pattern = 
    | PVariable of PVarName.t 

type expression = 
    | Void of Loc.t 
    | Variable of Loc.t * VarName.t
    | Constant of Loc.t * const
    | If of Loc.t * expression * expression * expression
    | Application of Loc.t * expression * expression list 
    | Let of Loc.t * VarName.t * expression * expression
    (* LetMut *)
    | Fn of Loc.t * VarName.t list * expression
    | Annotated of Loc.t * expression * ty
    | Sequence of Loc.t * expression list
    | Match of Loc.t * expression * (pattern * expression) list 
    | Record of Loc.t * (FieldName.t * expression) list 
    | TODO of Loc.t 

type claim = Claim of Loc.t * DefName.t * ty
type def = Def of Loc.t * DefName.t * expression   
type toplevel = Toplevel of claim list * def list * expression list