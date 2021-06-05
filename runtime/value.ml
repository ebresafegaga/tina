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
    | VVariant of VarName.t * value list 

let rec pp_value v = 
    match v with 
    | VUnit -> "(void)"
    | VInteger i -> Int.to_string i
    | VString s -> Printf.sprintf "%s%s%s" {|"|} s {|"|} 
    | VFloat f -> Float.to_string f 
    | VBool b -> Bool.to_string b
    | VClosure clo -> "<fun>" (* unfortunately, we can't inspect clo *)
    
    | VRecord (name, fields) ->
        let fields_pp = 
            fields  
            |> List.map (fun (name, value) -> 
                Printf.sprintf " %s: %s" (FieldName.to_string name) (pp_value value))
            |> String.concat ","
        in
        Printf.sprintf "%s {%s }" (DataName.to_string name) fields_pp 

    | VVariant (name, []) -> VarName.to_string name
    | VVariant (name, values) -> 
        let values_pp = values |> List.map pp_value |> String.concat ", " in
        Printf.sprintf "%s (%s)" (VarName.to_string name) values_pp