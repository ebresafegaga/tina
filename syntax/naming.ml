
module type ID = sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool

    val compare : t -> t -> int
    val pp : t -> string
    val fresh: string -> t 
end

module StringID = struct
    type t = string
    let of_string x = x
    let to_string x = x
    let ( = ) = String.equal
    let compare x y = if x = y then 0 else if x > y then 1 else -1  
    let pp = to_string
    let fresh =
      let state = ref 0 in
      fun s ->
        let v = Printf.sprintf "%s_%d" s !state in
        incr state;
        of_string v
end

module VarName : ID = StringID
module PVarName : ID = StringID
module DefName : ID = StringID
module FieldName : ID = StringID
module DataName : ID = StringID
module CtorName : ID = StringID
