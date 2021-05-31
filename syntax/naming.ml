
module type ID = sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
end

module StringID = struct
    type t = string
    let of_string x = x
    let to_string x = x
    let ( = ) = String.equal
end

module VarName : ID = StringID
module PVarName : ID = StringID
module DefName : ID = StringID
module FieldName : ID = StringID
