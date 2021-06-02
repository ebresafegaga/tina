


type t = 
    | VUnit 
    | VInteger of int 
    | VString of string 
    | VFloat of float
    | VBool of bool 
    | VClosure  (* env *)
    | VRecord 

type builtin = t list -> t

let plus : builtin = function 
    | [VInteger x; VInteger y] -> VInteger (x + y) 
    | _ -> failwith ""