

open Syntax
open Naming    

module T = Type

type t = (VarName.t * Type.t) list

let assume ctx x t = (x, t) :: ctx

let assume_list ctx xs ts =
  let alist = List.combine xs ts in
  alist @ ctx

let lookup ctx x =
  match List.assoc x ctx with
  | value -> value
  | exception Not_found ->
    let msg = Printf.sprintf "Unbound Variable %s" (VarName.to_string x) in
    Errors.runtime msg

let is_recordty = function
  | T.TyRecord _ -> true
  | _ -> false 

(* (FieldName.t * t) list *)
let rec lookup_record ctx name =
  let name' = name |> DataName.to_string |> VarName.of_string in
  match ctx with
  | [] ->
    let msg = Printf.sprintf "Unknown Record %s" (VarName.to_string name') in
    Errors.runtime msg
  | (x, t) :: ctx when x = name' ->
    (match t with
    | T.TyRecord r -> r
    | _ -> lookup_record ctx name)
  | (_x, _t) :: ctx -> lookup_record ctx name
   
let rec lookup_variant ctx name =
  let name' = name |> DataName.to_string |> VarName.of_string in
  match ctx with
  | [] ->
    let msg = Printf.sprintf "Unknown Variant %s" (VarName.to_string name') in
    Errors.runtime msg
  | (x, t) :: ctx when x = name' ->
    (match t with
    | T.TyVariant v -> v, (v |> List.find (fun {T.label; _} -> label = name)).fields
    | _ -> lookup_variant ctx name)
  | (_x, _t) :: ctx -> lookup_variant ctx name
  
let empty = []
