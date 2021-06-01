
module type ID = sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
    val pp : t -> string
end

module StringID = struct
    type t = string
    let of_string x = x
    let to_string x = x
    let ( = ) = String.equal
    let pp = to_string
end

module VarName : ID = StringID
module PVarName : ID = StringID
module DefName : ID = StringID
module FieldName : ID = StringID
module VariantName : ID = StringID
