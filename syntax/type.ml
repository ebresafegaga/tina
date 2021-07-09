open Naming
    
type t = 
    | TyBool
    | TyString
    | TyNat
    | TyInt
    | TyFloat
    | TyUnit
    | TyArrow of t list * t
    | TyRecord of (FieldName.t * t) list
    | TyTuple of t list
       
