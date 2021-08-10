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
open Utility

let d = Loc.dummy
let fresh = VarName.fresh              

module A = DesugarData
    
type t =
  | LitBool of Loc.t * bool 
  | LitInteger of Loc.t * int 
  | LitFloat of Loc.t * float 
  | LitString of Loc.t * string
  | Variable of Loc.t * VarName.t
  | If of Loc.t * t * t * t
  | Application of Loc.t * t * t list 
  | Let of Loc.t * VarName.t * t * t
  | Fn of Loc.t * VarName.t list * t
  | Record of Loc.t * (FieldName.t * t) list
  | RecordIndex of Loc.t * t * FieldName.t
  | Absurd of string * t

type toplevel = Def of Loc.t * VarName.t * t | Expression of t

let is_pvariable = function
  | A.PVariable _ -> true
  | _ -> false

let is_simple_pattern = function
  | A.PVariable _ | A.PString _
  | A.PInteger _ | A.PBool _ -> true
  | A.PRecord _  -> false

let rec freshen pats =
  match pats with
  | A.PVariable name :: pats ->
    let row, frech = freshen pats in
    A.PVariable name :: row, frech
  | pat :: pats ->
    let var = fresh "fresh" in
    let row, frech = freshen pats in
    (A.PVariable var :: row), (var, pat) :: frech
  | [] -> [], []

(** [g body frontier] consumes a [body] expression and a [frontier]
   which is represented a list of an association of variable and
   patterns. This is basically used to assign fresh variables name to
   recursive/nested patterns (and later pattern match on the variable)
   in order to un-nest recursive/nested patterns. *)
let rec g body frontier =
  let g = g body in
  let variable name = A.Variable (d, name) in
  let case expr clauses = A.Case (d, expr, clauses) in
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
    let tag = List.hd pats in
    let pats = List.tl pats in
    let names = List.map fst pats in
    let pats = List.map snd pats in
    let pats, front = freshen pats in
    let frontier = front @ frontier in
    let name_pat = tag :: List.combine names pats in
    case (variable name)
      [A.PRecord name_pat, g frontier]

(** [top pat body] is like the entry point to the [g] transformation. 
    It assumes [pat] is not the outer pattern of a case expression. 
    So it transforms patterns the way [g] should. *)
let rec top pat body =
  match pat with
  | A.PVariable _ 
  | A.PBool _
  | A.PInteger _
  | A.PString _ -> pat, g body []
  | A.PRecord pats when pats |> List.for_all (snd >> is_pvariable) -> pat, g body []
  | A.PRecord pats ->
    (* don't reduce the first pattern in pat because it is the tag *)
    let tag = List.hd pats in
    let pats = List.tl pats in
    let names = List.map fst pats in
    let pats = List.map snd pats in
    let pats, frontier = freshen pats in
    let name_pat = List.combine names pats in
    A.PRecord (tag :: name_pat), g body frontier (* add the tag back *)

let if' p pt pf = If (Loc.dummy, p, pt, pf)
let app f args = Application (Loc.dummy, f, args)
let let' name expr body = Let (Loc.dummy, name, expr, body)
let var name = Variable (d, VarName.of_string name)
let record_index record name = RecordIndex (d, record, FieldName.of_string name)
let equal = var "equal"

let expr_of_pat = function
  | A.PBool b -> LitBool (d, b)
  | A.PInteger i -> LitInteger (d, i)
  | A.PString s -> LitString (d, s)
  | A.PVariable _ | A.PRecord _ -> Errors.runtime "expr_of_pat: expected a constant pat"

let var_of_pat = function
  | A.PVariable v -> v
  | _ -> Errors.runtime "var_of_pat: expected a variable pattern"

let rec transform0 expr =
  match expr with
  | A.Case (loc, expr, clauses) ->
    let clauses =
      clauses
      |> List.map (fun (pattern, body) ->
          let body = transform0 body in
          top pattern body)
    in
    A.Case (loc, transform0 expr, clauses)
  | A.LitBool _
  | A.LitInteger _
  | A.LitFloat _
  | A.LitString _
  | A.Variable _ -> expr
  | A.If (loc, p, pt, pf) -> A.If (loc, transform0 p, transform0 pt, transform0 pf)
  | A.Application (loc, f, args) ->
    let args = args |> List.map transform0 in
    A.Application (loc, transform0 f, args)
  | A.Let (loc, A.PVariable name, expr, body) ->
    A.Let (loc, A.PVariable name, transform0 expr, transform0 body)
  | A.Let (loc, pat, expr, body) ->
    (* transform a let with pattern to a case expression *)
    let e = A.Case (loc, expr, [pat, body]) in
    transform0 e
  | A.Fn (loc, vars, body) -> A.Fn (loc, vars, transform0 body)
  | A.Annotated (loc, e, ty) -> A.Annotated (loc, transform0 e, ty)
  | A.Sequence (loc, a, b) -> A.Sequence (loc, transform0 a, transform0 b)
  | A.Record (loc, fields) ->
    let fields = fields |> List.map (fun (name, e) -> name, transform0 e) in
    A.Record (loc, fields)
  | A.RecordIndex (loc, expr, name) -> A.RecordIndex (loc, transform0 expr, name)
  | A.Absurd (s, e) -> A.Absurd (s, e)

let rec transform1 expr =
  match expr with
  | A.Variable (loc, name) -> Variable (loc, name)
  | A.LitBool (loc, b) -> LitBool (loc, b)
  | A.LitInteger (loc, i) -> LitInteger (loc, i)
  | A.LitFloat (loc, f) -> LitFloat (loc, f)
  | A.LitString (loc, s) -> LitString (loc, s)
  | A.If (loc, p, pt, pf) -> If (loc, transform1 p, transform1 pt, transform1 pf)
  | A.Application (loc, f, args) ->
    let args = args |> List.map transform1 in
    Application (loc, transform1 f, args)
  | A.Let (loc, var, expr, body) ->
    (* assumes all patterns are now variables *)
    let var = var_of_pat var in
    Let (loc, var, transform1 expr, transform1 body)
  | A.Fn (loc, vars, body) -> Fn (loc, vars, transform1 body) 
  | A.Annotated (_, e, _) -> transform1 e
  | A.Sequence (_loc, a, b) ->
    let' (fresh "seq") (transform1 a)
      (transform1 b)
  | A.Case (_loc, expr, clauses) -> case (transform1 expr) clauses
  | A.Record (loc, names) ->
    let names = names |> List.map (fun (name, e) -> name, transform1 e) in
    Record (loc, names)
  | A.RecordIndex (loc, expr, index) ->
    RecordIndex (loc, transform1 expr, index)
  | A.Absurd (s, e) -> Absurd (s, transform1 e)

and case e clauses =
  match clauses with
  | [] -> Absurd ("Pattern match failure", LitInteger (d, 0))
  | (A.PVariable x, body) :: _rest ->
    (* if' (LitBool (d, true))
       (let' x e (transform1 body))
       (case e rest) *)
    (let' x e (transform1 body)) (* it's a pattern variable - it's always going to match *)
  | (A.PInteger q, body) :: rest ->
    let q = LitInteger (d, q) in
    let predicate = app equal [q; e] in
    if' predicate
      (transform1 body)
      (case e rest)
  | (A.PString s, body) :: rest ->
    let s = LitString (d, s) in
    let predicate = app equal [s; e] in
    if' predicate
      (transform1 body)
      (case e rest)
  | (A.PBool b, body) :: rest ->
    let b = LitBool (d, b) in
    let predicate = app equal [b; e] in
    if' predicate
      (transform1 body)
      (case e rest)
  | (A.PRecord pats, body) :: rest ->
    let e_0 = record_index e "0" in
    let v_0 = pats |> List.map snd |> List.hd |> expr_of_pat in
    let pats = pats |> List.tl |> List.map (fun (n, p) -> FieldName.to_string n, var_of_pat p) in
    (* instead of disturbing the current lexical scope with potentially unused 
       (variable) bindings, we can substitue each variable usage in `body` with 
       the coresponding record-index operation. *)
    let predicate_true =
      List.fold_right (fun (field, variable) s -> let' variable (record_index e field) s)
        pats
        (transform1 body)
    in
    let predicate = app equal [e_0; v_0] in
    if' predicate
      predicate_true
      (case e rest)

(* tbh this second pass is *very* unecessary;
   the first pass is the real deal. in fact, 
   the seperation of the two passes is the source 
   of the bug in this pattern matching compiler. 

                how? why? which bug?

    bug: there is no backtracking for a failure when 
         compiling a nested pattern case. when one of the
         inner pattern fails, it should continue by checking 
         the next pattern-expression clause. the current 
         implementation doesn't.
   why?: first of all, the two transforms operate on *different* 
         syntax trees. also, `transform0` doesn't have access to 
         the next branch (i.e pattern-expression clause), so what 
         should it do when a nested pattern its un-nesting fails? 
         NOTHING. because it doens't have access to this contextual 
         information.

   so please fix this bug (if you understand, of course.) *)
let g = transform0 >> transform1

let rec handle_toplevel = function
  | [] -> []
  | A.Def (loc, name, expr) :: rest -> Def (loc, name, g expr) :: handle_toplevel rest
  | A.Expression e :: rest -> Expression (g e) :: handle_toplevel rest
  | (A.VariantDef _ | A.RecordDef _ | A.AbilityDef _  | A.Claim _) :: rest -> handle_toplevel rest

(* boilerplate pretty pprinting stuff *)

let pp_list es f = es |> List.map f |> String.concat ", "

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
      (VarName.to_string var)
      (pp_expression value)
      (pp_expression body)
  | Fn (_loc, names, body) ->
    Printf.sprintf "fn (%s) %s"
      (pp_list names VarName.to_string)
      (pp_expression body)
  | Record (_loc, fes) -> 
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
  | Def (_loc, name, expr) -> 
    Printf.sprintf "def %s = %s"
      (VarName.to_string name)
      (pp_expression expr)
  | Expression expr -> pp_expression expr