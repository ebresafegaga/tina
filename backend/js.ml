open Syntax
open Naming    
open Runtime
open Utility    

module A = KNormal

type expression =
  | LitBool of bool
  | LitInteger of int
  | LitFloat of float 
  | LitString of string
  | Variable of string
  | Application of expression * expression list
  | Fn of VarName.t list * statement list
  | Record of (VarName.t * expression) list
  | RecordIndex of expression * VarName.t

and statement =
  | If of expression * statement list * statement list
  | Return of expression option
  | Let of VarName.t * expression

type toplevel =
  | Def of string * expression
  | Expression of expression

let variable name = Variable (VarName.to_string name)

let rec gexpr = function
  | A.Variable name -> variable name
  | A.LitBool b -> LitBool b
  | A.LitInteger i -> LitInteger i
  | A.LitFloat f -> LitFloat f
  | A.LitString s -> LitString s
  | A.Fn (args, e) -> Fn (args, gstate e)
  | A.Application (f, args) ->
    let args = args |> List.map variable in 
    let f = variable f in
    Application (f, args)
  | A.Record fields ->
    let var = FieldName.to_string >> VarName.of_string in
    let fields = List.map (fun (v, e) -> var v , variable e) fields in
    Record fields
  | A.RecordIndex (name, index) ->
    let name = variable name in
    let var = FieldName.to_string >> VarName.of_string in
    RecordIndex (name, var index)
  | A.Absurd (s, _e) ->
    let absurd = Variable "absurd" in
    Application (absurd, [LitString s])
      
  (* these are statements in javascript *)
  | Let _ | If _ -> Errors.runtime "JS code gen: Expected but got a statement"

and gstate expr =
  match expr with 
  | A.LitBool _ | A.LitFloat _
  | A.LitString _ | A.LitInteger _
  | A.Variable _ | A.Application _
  | Record _ | RecordIndex _ | A.Absurd _
  | A.Fn _ -> [Return (Some (gexpr expr))]
  | A.Let (x, expr, body) ->
    Let (x, gexpr expr) :: gstate body
  | A.If (p, pt, pf) ->
    let p = variable p in
    [If (p, gstate pt, gstate pf)]

let handle_top = function
  | A.Def (name, expr) -> Def (VarName.to_string name, gexpr expr)
  | A.Expression e -> Expression (gexpr e)

let handle_toplevel = List.map handle_top

let pp_list es f = es |> List.map f |> String.concat ", "
let combine_statement es = es |> String.concat "; "                     
                     
let rec gen_expression = function
  | LitInteger i -> string_of_int i
  | LitBool b -> string_of_bool b
  | LitFloat f -> string_of_float f
  | LitString s -> Printf.sprintf {|"%s"|} s
  | Variable s -> s
  | Application (f, args) -> Printf.sprintf "%s (%s)" (gen_expression f) (pp_list args gen_expression)
  | Fn (args, body) ->
    Printf.sprintf  {| (%s) => { %s } |} (pp_list args VarName.to_string)
      (combine_statement (List.map gen_statement body))
  | Record fields ->
    Printf.sprintf "{ %s }" (pp_list fields (fun (name, expr) ->
        Printf.sprintf "%s: %s" (VarName.to_string name) (gen_expression expr)))
  | RecordIndex (expr, index) ->
    Printf.sprintf "%s[%s]" (gen_expression expr) (VarName.to_string index)

and gen_statement = function
  | Return (Some e) -> Printf.sprintf "return %s;" (gen_expression e)
  | Return None -> "return;"
  | If (p, pt, pf) ->
    Printf.sprintf
      {| if (%s) { 
         %s 
       } else { 
           %s 
       } |} (gen_expression p) (combine_statement (List.map gen_statement pt))
      (combine_statement (List.map gen_statement pf))
  | Let (x, e) -> Printf.sprintf "let %s = %s" (VarName.to_string x) (gen_expression e)
