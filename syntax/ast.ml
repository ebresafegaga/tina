open Utils
open Naming

type ty = 
    | TyString 
    | TyNat 
    | TyInt
    | TyFloat 
    | TyUnit
    | TyArrow of ty list * ty 
    (* Record type *)

type const = 
    | CBool of bool 
    (* | CNat of int  *)
    | CInt of int
    | CFloat of float 
    | CString of string 

type pattern = 
    | PVariable of PVarName.t 

    (* we need a variable type which enacpsulates 
      VarName.t DefName.t ... *)
type expression = 
    | Unit of Loc.t 
    | Variable of Loc.t * VarName.t
    | Constant of Loc.t * const
    | If of Loc.t * expression * expression * expression
    | Application of Loc.t * expression * expression list 
    | Let of Loc.t * VarName.t * expression * expression
    (* LetMut maybe? *)
    | Fn of Loc.t * VarName.t list * expression
    | Annotated of Loc.t * expression * ty
    | Sequence of Loc.t * expression list
    | Match of Loc.t * expression * (pattern * expression) list 
    | Record of Loc.t * (FieldName.t * expression) list 
    | TODO of Loc.t 

(* also data decls and abilities *)
type toplevel = 
    | Claim of Loc.t * DefName.t * ty
    | Def of Loc.t * DefName.t * expression
    | Expression of expression

let f x b = 
    let x = 34 in 


fun () -> ()