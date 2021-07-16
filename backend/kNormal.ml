open Utility
open Syntax
open Naming
open Runtime    

module A = DesugarCase

type t =
  | LitBool of bool 
  | LitInteger of  int 
  | LitFloat of float 
  | LitString of string
  | Variable of VarName.t
  | If of VarName.t * t * t
  | Application of VarName.t * VarName.t list 
  | Let of VarName.t * t * t
  | Fn of VarName.t list * t
  | Record of (FieldName.t * VarName.t) list
  | RecordIndex of VarName.t * FieldName.t
  | Absurd of string * t

type toplevel = Def of VarName.t * t | Expression of t

(* this function does all the magic *)
let insert_let expr k =
  match expr with
  | Variable x -> k x
  | _ ->
    let x = VarName.fresh "x" in
    let e = k x in
    Let (x, expr, e)

let rec g0 = function
  | A.LitBool (_loc, b) -> LitBool b
  | A.LitInteger (_loc, i) -> LitInteger i
  | A.LitFloat (_loc, f) -> LitFloat f
  | A.LitString (_loc, s) -> LitString s
  | A.Variable (_loc, x) -> Variable x
  | A.If (_loc, p, pt, pf) ->
    insert_let (g0 p)
      (fun x ->
         If (x, (g0 pt), (g0 pf)))
  | A.Application (_loc, f, args) ->
    insert_let (g0 f)
      (fun f ->
         sequence args
           (fun args ->
              Application (f, args)))
  | A.Let (_loc, x, expr, body) -> Let (x, g0 expr, g0 body)
  | A.Fn (_loc, args, body) -> Fn (args, g0 body)
  | A.Record (_loc, fields) ->
    let tags, exprs = List.split fields in
    sequence exprs
      (fun xs ->
         Record (List.combine tags xs))
  | A.RecordIndex (_loc, expr, index) ->
    insert_let (g0 expr)
      (fun x ->
         RecordIndex (x, index))
  | A.Absurd (s, e) -> Absurd (s, g0 e)

(* A.t list -> (VarName.t list -> t) -> t*)
and sequence es k =
  match es with
  | [] -> k []
  | e :: es ->
    insert_let (g0 e)
      (fun x ->
         sequence es
           (fun xs ->
              k (x :: xs)))

(* flatten nested let bindings *)
let rec g1 = function
  | If (p, pt, pf) -> If (p, g1 pt, g1 pf)
  | Fn (args, body) -> Fn (args, g1 body)
  | Let (x, expr, body) ->
    let rec insert = function
      | Let (y, yexpr, ybody) -> Let (y, yexpr, insert ybody)
      | e -> Let (x, e, g1 body)
    in
    insert (g1 expr)
  | e -> e


let g = g0 >> g1

let handle_top = function
  | A.Def (_loc, name, body) -> Def (name, g body)
  | A.Expression e -> Expression (g e)

let handle_toplevel = List.map handle_top

(* boilerplate pretty pprinting stuff *)

let pp_list es f = es |> List.map f |> String.concat ", "

let rec pp_expression = function
  | LitBool (b) -> Bool.to_string b
  | LitInteger ( i) -> Int.to_string i
  | LitFloat ( f) -> Float.to_string f
  | LitString ( s) -> s
  | Variable ( v) -> VarName.to_string v
  | If ( pred, tru, fals) ->
    Printf.sprintf "if %s then %s else %s"
      (VarName.to_string pred)
      (pp_expression tru)
      (pp_expression fals)
  | Application ( rand, es) ->
    Printf.sprintf "%s (%s)"
      (VarName.to_string rand)
      (pp_list es VarName.to_string)
  | Let ( var, value, body) ->
    Printf.sprintf "let %s = %s; %s"
      (VarName.to_string var)
      (pp_expression value)
      (pp_expression body)
  | Fn ( names, body) ->
    Printf.sprintf "fn (%s) %s"
      (pp_list names VarName.to_string)
      (pp_expression body)
  | Record ( fes) -> 
    let f (field, expr) =
      Printf.sprintf "%s: %s"
        (FieldName.to_string field)
        (VarName.to_string expr)
    in
    Printf.sprintf "{%s}"
      (pp_list fes f)
  | RecordIndex ( expr, name) ->
    Printf.sprintf "%s.%s"
      (VarName.to_string expr)
      (FieldName.to_string name)
  | Absurd (s, e) ->
    Printf.sprintf "absurd (%s, %s)" s (pp_expression e)

let pp_toplevel = function
  | Def ( name, expr) -> 
    Printf.sprintf "def %s = %s"
      (VarName.to_string name)
      (pp_expression expr)
  | Expression expr -> pp_expression expr
