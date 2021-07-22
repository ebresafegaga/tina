

open Syntax
open Naming    

module T = Type

type t = (VarName.t * Type.t) list

let assume ctx x t = (x, t) :: ctx

let assume_list ctx xs ts =
  List.fold_right2 (fun name ty ctx -> Ctx.assume ctx name ty) xs ts ctx
  (* let alist = List.combine xs ts in
     alist @ ctx *)

let lookup ctx x =
  match List.assoc x ctx with
  | value -> value
  | exception Not_found ->
    let msg = Printf.sprintf "Unbound Variable %s" (VarName.to_string x) in
    Errors.runtime msg

let lookup_claim ctx x =
  match List.assoc x ctx with
  | value -> value
  | exception Not_found ->
    let msg = Printf.sprintf "You must specify a type for the toplevel declaration %s" (VarName.to_string x) in
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
    | T.TyVariant v -> v, (v |> List.find (fun { T.label; _ } -> label = name)).fields
    | _ -> lookup_variant ctx name)
  | (_x, _t) :: ctx -> lookup_variant ctx name

let is_bound ctx name = lookup ctx name |> ignore
  
let empty = []

let default =
  ["+", T.TyArrow ([T.TyInt; T.TyInt], T.TyInt)]
  |> List.map (fun (n, t) -> VarName.of_string n, t)

let pp_ctx =
  List.map (fun (name, ty) ->
      Printf.sprintf "%s : %s" (VarName.to_string name) (T.pp_ty ty))
