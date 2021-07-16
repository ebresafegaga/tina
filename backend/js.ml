open Syntax
open Naming    
open Runtime

module A = KNormal

type expression =
  | LitBool of bool
  | LitInteger of int
  | LitFloat of float 
  | LitString of string
  | Variable of string
  | Application of expression * expression list
  | Fn of VarName.t list * statement list
  | Record of (VarName.t * expression) list
  | RecordIndex of expression * VarName.t

and statement =
  | If of expression * expression * expression
  | Return of expression option
  | Let of VarName.t * expression

type toplevel =
  | Def of string * statement list
  | Expression of expression
