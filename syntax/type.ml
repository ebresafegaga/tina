open Naming

type kind = KType | KComp | KEffect | KHandler
    
type t =
   (* A = Bool | Str | Nat | Int | Float | Unit | Arr 
         | Record | Tuple | Variant *)
    | TyBool
    | TyString
    | TyNat
    | TyInt
    | TyFloat
    | TyUnit
    | TyArrow of t list * t (* a ... -> b *)
    | TyRecord of record
    | TyTuple of t list
    | TyVariant of variant list
    | TyEffect of record (* E = {l ...} *)
    | TyComp  of { pure: t; impure: t } (* C = A ! E *)
    | TyHandler of t * t (* H = M ~> N*)
       
and variant = { label: DataName.t; fields: t list }

and record = (FieldName.t * t) list

let tyequal : t -> t -> bool = (=) (* for now *)

let pp_list es f = es |> List.map f |> String.concat ", "

let rec pp_ty = function
  | TyNat -> "Nat"
  | TyString -> "String"
  | TyInt -> "Int"
  | TyFloat -> "Float"
  | TyBool -> "Bool"
  | TyUnit -> "Unit"
  | TyTuple ts -> Printf.sprintf "(%s)" (pp_list ts pp_ty)
  | TyRecord ts ->
    Printf.sprintf "{%s}" @@
    pp_list ts (fun (n, t) -> Printf.sprintf "claim %s %s" (FieldName.to_string n) (pp_ty t))
  | TyArrow (ts, t) -> Printf.sprintf "(%s) -> %s" (pp_list ts pp_ty) (pp_ty t)
  | _ -> ""
