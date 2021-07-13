

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
  | LitTodo of Loc.t 
  | LitUnit of Loc.t 
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

let rec g_pat = function
  | A.PVariable name -> PVariable name 
  | A.PBool b -> PBool b
  | A.PString s -> PString s
  | A.PInteger i -> PInteger i
  | A.PRecord (_name, pats) ->
    let len = List.length pats in
    let tag = "tag" |> FieldName.of_string in 
    let tag_value = PInteger 0 in
    let names = List.init len (fun i -> i |> string_of_int |> FieldName.of_string) in
    let fields = List.map (snd >> g_pat) pats in 
    let fields = (tag, tag_value) :: List.combine names fields in
    PRecord fields
  | A.PVariant (name, pats) ->
    let name = name |> VarName.to_string |> DataName.of_string in
    let index = lookup_variant name in
    let len = List.length pats in
    let tag = "tag" |> FieldName.of_string in 
    let tag_value = PInteger index in
    let names = List.init len (fun i -> i |> string_of_int |> FieldName.of_string) in
    let fields = List.map g_pat pats in 
    let pats = (tag, tag_value) :: List.combine names fields in
    PRecord pats
  | A.PTuple pats ->
    let len = List.length pats in
    let tag = "tag" |> FieldName.of_string in 
    let tag_value = PInteger 0 in
    let names = List.init len (fun i -> i |> string_of_int |> FieldName.of_string) in
    let pats = List.map g_pat pats in 
    let pats = (tag, tag_value) :: List.combine names pats in
    PRecord pats

let rec g expr =
  match expr with
  | A.Tuple (loc, fields) ->
    let len = List.length fields in
    let tag = "tag" |> FieldName.of_string in 
    let tag_value = LitInteger (loc, 0) in
    let names = List.init len (fun i -> i |> string_of_int |> FieldName.of_string) in
    let fields = List.map g fields in 
    let fields = (tag, tag_value) :: List.combine names fields in
    Record (loc, fields)
  | A.Variant (loc, name, fields) ->
    let index = lookup_variant name in
    let len = List.length fields in
    let tag = "tag" |> FieldName.of_string in 
    let tag_value = LitInteger (loc, index) in
    let names = List.init len (fun i -> i |> string_of_int |> FieldName.of_string) in
    let fields = List.map g fields in 
    let fields = (tag, tag_value) :: List.combine names fields in
    Record (loc, fields)
  | A.Record (loc, _name, fields) ->
    (* we need to transform all uses of fields to their integer positions *)
    let len = List.length fields in
    let tag = "tag" |> FieldName.of_string in 
    let tag_value = LitInteger (loc, 0) in
    let names = List.init len (fun i -> i |> string_of_int |> FieldName.of_string) in
    let fields = List.map (snd >> g) fields in 
    let fields = (tag, tag_value) :: List.combine names fields in
    Record (loc, fields)
  | A.LitTodo loc -> LitTodo loc
  | A.LitUnit loc -> LitUnit loc
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
    let indices = List.init len Fun.id in
    let tbl = List.combine names indices in
    record_table := tbl @ !record_table;
    RecordDef (loc, name, body) 
  | A.AbilityDef (loc, name, tys) -> AbilityDef (loc, name, tys)
  | A.Expression _ -> failwith ""

let handle_toplevel = List.map toplevel

(* 
  we need a table to map variants name to their correspomding integr values
*)

(* | TAG | ... 
   for variants the tag is an integer representing it's position in the variants definition 
   for tuples the tag is 0 
   for records is also zero *)
