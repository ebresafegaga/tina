
type base = 
    | String 
    | Nat 
    | Int
    | Float 

type t = 
    | Base of base 
    | Arrow of t list * t 