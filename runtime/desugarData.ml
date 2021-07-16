

(* transform all tuple and variant patterns and expressions 
   into record patterns and expressions 
   
   { 0 : ...
     1 : ...
     2 : ... 
     .
     .
     .       
     n : ... }
   
   field 0 is a tag, field 1 to n represent the field 
   that they hold 
*)

open Syntax
open Naming
open Utility

module A = Ast

(* contaains bindings of varinats names to positional indexes *)
let variant_table : (DataName.t * int) list ref = ref []
let lookup_variant name =
  match List.assoc name !variant_table with
  | value -> value
  | exception Not_found ->
    Errors.runtime @@ Printf.sprintf "desugar datatypes: the variant name %s undefined"
      (DataName.to_string name)

let record_table : (FieldName.t * int) list ref = ref []
let lookup_record name =
  match List.assoc name !record_table with
  | value -> value
  | exception Not_found ->
    Errors.runtime @@ Printf.sprintf "desugar datatypes: the record name %s undefined"
      (FieldName.to_string name)

type pattern = 
  | PInteger of int 
  | PString of string 
  | PBool of bool
  | PVariable of VarName.t
  | PRecord of (FieldName.t * pattern) list
  
type t =
                 
  | LitBool of Loc.t * bool 
  | LitInteger of Loc.t * int 
  | LitFloat of Loc.t * float 
  | LitString of Loc.t * string
                 
  | Variable of Loc.t * VarName.t
  | If of Loc.t * t * t * t
  | Application of Loc.t * t * t list 
  | Let of Loc.t * pattern * t * t
  | Fn of Loc.t * VarName.t list * t
  | Annotated of Loc.t * t * A.ty 
  | Sequence of Loc.t * t * t
  | Case of Loc.t * t * (pattern * t) list
  | Record of Loc.t * (FieldName.t * t) list
  | RecordIndex of Loc.t * t * FieldName.t
  | Absurd of string * t

type toplevel =
  | Claim of Loc.t * VarName.t * A.ty
  | Def of Loc.t * VarName.t * t
  | VariantDef of Loc.t * DataName.t * (VarName.t * A.ty list) list
  | RecordDef of Loc.t * DataName.t * (FieldName.t * A.ty) list
  | AbilityDef of Loc.t * VarName.t * A.ty list
  | Expression of t


(* [reduce_to_record f g xs tag] takes a contructor function [f], a 
   transformation function [g] a list of transformable items [xs], and a 
   [tag] which is already tranformed. it then created an a list of pairs 
   of transformed items tagged with a field name. the list starts with [(0, tag)]
   then all the other transformed items folllow, paired up with their respective 
   index. the constructor function [f] is then applied to this a-list to get whatever 
   expression we desire. *)
let transform f g xs tag = 
  let len = 1 + List.length xs in  (* add one to accomodate the tag *)
  let names = List.init len (string_of_int >> FieldName.of_string) in
  let fields = tag :: List.map g xs in (* add the tag to the fields *)
  let fields = List.combine names fields in
  f fields
    
let reduce_to_precord = transform (fun p -> PRecord p)
    
(* we can also decide to take an arbitrary `Loc.t` 
   instead of using `Loc.dummy` *)    
let reduce_to_erecord = transform (fun e -> Record (Loc.dummy, e))

let rec g_pat = function
  | A.PVariable name -> PVariable name 
  | A.PBool b -> PBool b
  | A.PString s -> PString s
  | A.PInteger i -> PInteger i
  | A.PRecord (_name, pats) ->
    let tag = (PInteger 0) in
    let pats = List.map snd pats in (* forget about field names *)
    reduce_to_precord g_pat pats tag
  | A.PVariant (name, pats) ->
    let name = name |> VarName.to_string |> DataName.of_string in
    let index = lookup_variant name in
    let tag = PInteger index in
    reduce_to_precord g_pat pats tag
  | A.PTuple pats ->
    let tag = PInteger 0 in
    reduce_to_precord g_pat pats tag

let rec g expr =
  match expr with
  | A.Tuple (loc, fields) ->
    let tag = LitInteger (loc, 0) in
    reduce_to_erecord g fields tag
  | A.Variant (loc, name, fields) ->
    let index = lookup_variant name in
    let tag = LitInteger (loc, index) in
    reduce_to_erecord g fields tag
  | A.Record (loc, _name, fields) ->
    (* we need to transform all uses of fields to their integer positions *)
    let fields = List.map snd fields in
    let tag = LitInteger (loc, 0) in
    reduce_to_erecord g fields tag
  | A.LitTodo loc -> Absurd ("Unreplaced TODO", LitInteger (loc, 0))
  | A.LitUnit loc -> LitInteger (loc, 0) (* zero is unit *)
  | A.LitBool (loc, b) -> LitBool (loc, b)
  | A.LitInteger (loc, i) -> LitInteger (loc, i)
  | A.LitFloat (loc, f) -> LitFloat (loc, f)
  | A.LitString (loc, s) -> LitString (loc, s)
  | A.Variable (loc, name) -> Variable (loc, name)
  | A.If (loc, p, pt, pf) -> If (loc, g p, g pt, g pf)
  | A.Application (loc, f, args) ->
    let args = List.map g args in 
    Application (loc, g f, args)
  | A.Let (loc, pat, expr, body) ->
    Let (loc, g_pat pat, g expr, g body)
  | A.Fn (loc, vars, body) -> Fn (loc, vars, g body)
  | A.Annotated (loc, expr, ty) -> Annotated (loc, g expr, ty)
  | A.Sequence (loc, a, b) -> Sequence (loc, g a, g b)
  | A.Absurd (s, e) -> Absurd (s, g e)

  | A.Case (loc, expr, clauses) ->
    let clauses = clauses |> List.map (fun (p, e) -> g_pat p, g e) in
    Case (loc, g expr, clauses)
  | A.RecordIndex (loc, expr, name) -> 
    let index = lookup_record name |> string_of_int |> FieldName.of_string in
    RecordIndex (loc, g expr, index)

  |A.Do (_, _, _)
  |A.Handle (_, _, _) -> failwith "leave this for now, fix after we implment the Type module"

let toplevel = function
  | A.Claim (loc, name, ty) -> Claim (loc, name, ty)
  | A.Def (loc, name, expr) -> Def (loc, name, g expr)
  | A.VariantDef (loc, name, body) ->
    let names = List.map (fst >> VarName.to_string >> DataName.of_string) body in
    let len = List.length body in
    let indices = List.init len Fun.id in
    let tbl = List.combine names indices in
    variant_table := tbl @ !variant_table;
    VariantDef (loc, name, body) 
  | A.RecordDef (loc, name, body) ->
    let names = List.map fst body in
    let len = List.length body in
    let indices = List.map succ @@ List.init len Fun.id in
    let tbl = List.combine names indices in
    record_table := tbl @ !record_table;
    RecordDef (loc, name, body) 
  | A.AbilityDef (loc, name, tys) -> AbilityDef (loc, name, tys)
  | A.Expression e -> Expression (g e)

let handle_toplevel = List.map toplevel

(* 
  we need a table to map variants name to their correspomding integr values
*)

(* | TAG | ... 
   for variants the tag is an integer representing it's position in the variants definition 
   for tuples the tag is 0 
   for records is also zero *)


let pp_list es f = es |> List.map f |> String.concat ", "
  
let rec pp_pattern = function
  | PInteger i -> Int.to_string i
  | PString s -> s
  | PVariable name -> VarName.to_string name
  | PBool b -> Bool.to_string b
  | PRecord es ->
    Printf.sprintf "{%s}" @@
    pp_list es (fun (name, pattern) -> Printf.sprintf "%s: %s" (FieldName.to_string name) (pp_pattern pattern))

let rec pp_expression = function 
  | LitBool (_loc, b) -> Bool.to_string b
  | LitInteger (_loc, i) -> Int.to_string i
  | LitFloat (_loc, f) -> Float.to_string f
  | LitString (_loc, s) -> s
  | Variable (_loc, v) -> VarName.to_string v
  | If (_loc, pred, tru, fals) ->
    Printf.sprintf "if %s then %s else %s"
      (pp_expression pred)
      (pp_expression tru)
      (pp_expression fals)
  | Application (_loc, rand, es) ->
    Printf.sprintf "%s (%s)"
      (pp_expression rand)
      (pp_list es pp_expression)
  | Let (_loc, var, value, body) ->
    Printf.sprintf "let %s = %s; %s"
      (pp_pattern var)
      (pp_expression value)
      (pp_expression body)
  | Fn (_loc, names, body) ->
    Printf.sprintf "fn (%s) %s"
      (pp_list names VarName.to_string)
      (pp_expression body)
  | Annotated (_loc, expr, ty) ->
    Printf.sprintf "(the %s %s)"
      (pp_expression expr)
      (A.pp_ty ty)
  | Sequence (_loc, a, b) ->
    Printf.sprintf "%s; %s;"
      (pp_expression a)
      (pp_expression b)
  | Case (_loc, expr, pes) -> (* pes - pattern, expression S *)
    let f (pat, expr) =
      Printf.sprintf "%s -> %s"
        (pp_pattern pat)
        (pp_expression expr)
    in
    Printf.sprintf "case %s { %s }" 
      (pp_expression expr)
      (pp_list pes f)
  | Record (_loc, fes) -> (* fes - field, expression S *)
    let f (field, expr) =
      Printf.sprintf "%s: %s"
        (FieldName.to_string field)
        (pp_expression expr)
    in
    Printf.sprintf "{%s}"
      (pp_list fes f)
  | RecordIndex (_loc, expr, name) ->
    Printf.sprintf "%s.%s"
      (pp_expression expr)
      (FieldName.to_string name)
  | Absurd (s, e) ->
    Printf.sprintf "absurd (%s, %s)" s (pp_expression e)

let pp_toplevel = function
  | Claim (_loc, name, ty) ->
    Printf.sprintf
      "claim %s %s"
      (VarName.to_string name)
      (A.pp_ty ty)
  | Def (_loc, name, expr) -> (* TODO: add a special case for fn *)
    Printf.sprintf "def %s = %s"
      (VarName.to_string name)
      (pp_expression expr)
  | Expression expr -> pp_expression expr
  | VariantDef _ | RecordDef _ | AbilityDef _ -> "<def>" (* for now *)
