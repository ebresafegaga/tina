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

let rec pp_value v = 
    match v with 
    | VUnit -> "(void)"
    | VInteger i -> Int.to_string i
    | VString s -> Printf.sprintf "%s%s%s" {|"|} s {|"|} 
    | VFloat f -> Float.to_string f 
    | VBool b -> Bool.to_string b
    | VClosure clo -> "<fun>"
    | VRecord (name, fields) ->
        let pp_fields = 
            fields  
            |> List.map (fun (name, value) -> 
                Printf.sprintf " %s: %s" (FieldName.to_string name) (pp_value value))
            |> String.concat ","
        in
        Printf.sprintf "%s {%s }" (DataName.to_string name) pp_fields 