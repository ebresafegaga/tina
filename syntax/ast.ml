open Utils


type base_ty = 
    | String 
    | Nat 
    | Int
    | Float 

type ty = 
    | Base of base_ty 
    | Arrow of ty list * ty 

type name = string

type const = 
    | Bool of bool 
    | Nat of int 
    | Int of int
    | Float of float 
    | String of string 

type pattern = 
    | PVariable of name

type expression = 
    | Void 
    | Variable of name 
    | Constant of const
    | If of expression * expression * expression
    | Application of expression * expression list 
    | Let of name * expression * expression
    | List of expression list 
    | Fn of name list * expression
    | Annotated of expression * ty
    | Sequence of expression list
    | Match of expression * (pattern * expression) list 
    | Record of (name * expression) list 
    | TODO
    
type toplevel = 
    | Claim of name * ty
    | Def of name * expression
    | Expression of expression