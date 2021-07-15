open Naming

type kind = KType | KComp | KEffect | KHandler
    
type t =
   (* A = Bool | Str | Nat | Int | Float | Unit | Arr 
         | Record | Tuple | Variant *)
    | TyBool
    | TyString
    | TyNat
    | TyInt
    | TyFloat
    | TyUnit
    | TyArrow of t list * t (* a ... -> b *)
    | TyRecord of (FieldName.t * t) list
    | TyTuple of t list
    | TyVariant
          
    | TyEffect (* E = {l ...} *)
    | TyComp  (* C = A ! E *)
    | TyHandler (* H = M ~> N*)
       
