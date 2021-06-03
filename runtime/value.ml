open Syntax
open Naming


type value = 
    | VUnit 
    | VInteger of int 
    | VString of string 
    | VFloat of float
    | VBool of bool 
    | VClosure of (value list -> (value, string) result)
    | VRecord of DataName.t * (FieldName.t * value) list 

let pp_value ppf v = 
    failwith ""