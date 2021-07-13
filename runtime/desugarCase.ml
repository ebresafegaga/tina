(* 
    DESUGARING CASE EPXRESIONS TO IF EXPRESIONS 

   - first we transform the recursive patterns to normal pattern match 
     with nested cases in the body to handle arbitrary levels of the 
     recursive pattern 

   - next, we transform multiple clauses to two clauses: 
     one for the head case and a variable case. in the body of 
     the head case, we pattern match on the variable and repeat 
     the same procedure for the rest clauses until we're done. 

  - now we have only two clauses, for each case, so this can 
    be straight forwardly transformed into and if expression. 

   notes: 
   - we might need a new data structure to represent the 
     transformed ast, without case expressions 

   - the transformation functions need to work nicely recursively

   Some (10, x) ~> 
     Some (x, y) -> 

   we need to implement a generic equality operation 
*)

open Syntax
open Naming

let d = Loc.dummy
let fresh = VarName.fresh          

module A = DesugarData
    
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
  | Let of Loc.t * VarName.t * t * t
  | Fn of Loc.t * VarName.t list * t
  (* | Annotated of Loc.t * t * A.ty  *)
  | Sequence of Loc.t * t * t
  | Record of Loc.t * DataName.t * (FieldName.t * t) list
  | RecordIndex of Loc.t * t * FieldName.t
  | Tuple of Loc.t * t list
  | Variant of Loc.t * DataName.t * t list
  | Absurd of string * t

let is_pvariable = function
  | A.PVariable _ -> true
  | _ -> false

let is_simple_pattern = function
  | A.PVariable _ | A.PString _
  | A.PInteger _ | A.PBool _ -> true
  | A.PRecord _  -> false

let variable name = A.Variable (d, name)
let case expr clauses = A.Case (d, expr, clauses)

let rec freshen pats =
  match pats with
  | A.PVariable name :: pats ->
    let row, frech = freshen pats in
    A.PVariable name :: row, frech
  | pat :: pats ->
    let var = fresh "%fresh" in
    let row, frech = freshen pats in
    (A.PVariable var :: row), (var, pat) :: frech
  | [] -> [], []

let rec g body frontier =
  let g = g body in
  match frontier with
  | [] -> body
  | (name, A.PVariable x) :: frontier ->
    case (variable name)
      [A.PVariable x, g frontier]
  | (name, A.PBool b) :: frontier ->
    case (variable name)
      [A.PBool b, g frontier]
  | (name, A.PInteger i) :: frontier ->
    case (variable name)
      [A.PInteger i, g frontier]
  | (name, A.PString s) :: frontier ->
    case (variable name)
      [A.PString s, g frontier]
  | (name, A.PRecord pats) :: frontier ->
    let names = List.map fst pats in
    let pats = List.map snd pats in
    let pats, front = freshen pats in
    let frontier = front @ frontier in
    let name_pat = List.combine names pats in
    case (variable name)
      [A.PRecord name_pat, g frontier]
  






