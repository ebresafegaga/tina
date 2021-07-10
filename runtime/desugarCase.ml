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

module A = Ast
    
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
  | Annotated of Loc.t * t * A.ty 
  | Sequence of Loc.t * t * t
  | Record of Loc.t * DataName.t * (FieldName.t * t) list
  | RecordIndex of Loc.t * t * FieldName.t
  | Tuple of Loc.t * t list
  | Variant of Loc.t * DataName.t * t list
  | Absurd of string

(*
let firstly = function
  | A.Case (loc, expr, [clause]) ->
    let z = VarName.fresh "z" in
    A.Case (loc, expr,
              [clause;
              A.PVariable z, Absurd "pattern match failure"])
  | A.Case (loc, expr, clause :: clauses) ->
    let z = VarName.fresh "z" in
    failwith ""
  | _ -> failwith "not yet implemented"
*)

(* 
   case v { 
      Some (Some (None, y), z) -> ...
               .
               .
               .
   }


   case v {
     Some (%fresh, z) ->
        case (%fresh) {
           Some (%fresh2, y) -> 
               case (%fresh2) {
                   None -> ...
               }
        }
   }
   
*)

let is_pvariable = function A.PVariable _ -> true | _ -> false
let is_complex = Fun.negate is_pvariable  

let rec g body pattern =
  match pattern with
  | A.PVariable _ -> `Simple (pattern, body)
  | A.PBool b -> `Case (A.PBool b, body)
  | A.PInteger i -> `Case (A.PInteger i, body)
  | A.PString s -> `Case (A.PString s, body)
  | A.PTuple pats ->
    let is_simple =
      pats
      |> List.map (g body)
      |> List.for_all (function `Simple _ -> true | _ -> false)
    in
    if is_simple then
      `Case (A.PTuple pats, body)
    else
      failwith ""
  | _ -> failwith ""



