
open Syntax
open Naming    
open Runtime

module A = DesugarCase

type t =
  | LitBool of bool
  | LitInteger of int
  | LitFloat of float 
  | LitString of string
  | Variable of string
  | If of t * t * t
  | Application of t * t list
  | Let of string * t
  | Fn of string list * t list
  | Record of (string * t) list
  | RecordIndex of t * int
  | Absurd of string

type toplevel = Def of string  * t | Expression of t

let pp_list es f = es |> List.map f |> String.concat ", "

let rec gen e =
  match e with
  | A.LitUnit _loc -> ""
  | A.LitString (_loc, s) -> Printf.sprintf {|"%s"|} s
  | A.LitBool (_loc, b) -> string_of_bool b
  | A.LitFloat (_loc, f) -> string_of_float f
  | A.LitInteger (_loc, i) -> string_of_int i
  | A.If (_loc, p, pt, pf) ->
    let p, pt, pf = gen p, gen pt, gen pf in
    Printf.sprintf "if (%s) { %s } else { %s }" p pt pf
  | A.Variable (_loc, x) -> Printf.sprintf "%s" (VarName.to_string x)
  | A.Let (_loc, x, expr, body) ->
    Printf.sprintf "let %s = %s; %s" (VarName.to_string x)
      (gen expr) (gen body)
  | A.Application (_loc, f, args) ->
    Printf.sprintf "%s (%s)" (gen f) (pp_list args gen)
  | A.Fn (_loc, args, body) ->
    Printf.sprintf "(%s) => { %s }" (pp_list args VarName.to_string)
      (gen body)
  | A.Record (_loc, fields) ->
    Printf.sprintf "{ %s }"
      (pp_list fields (fun (name, e) -> Printf.sprintf "%s: %s" (FieldName.to_string name) (gen e)))
  | A.RecordIndex (_loc, expr, index) ->
    Printf.sprintf "%s[%s]" (gen expr) (FieldName.to_string index)
  | A.Absurd (s, _e) ->
    Printf.sprintf "absurd (%s)" s

let gen_top = function
  | A.Def (_loc, name, expr) ->
    Printf.sprintf "const %s = %s" (VarName.to_string name)
      (gen expr)
  | A.Expression (e) -> gen e

let gen_toplevel = List.map gen_top
