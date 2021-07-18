open Syntax
open Naming
module A = Ast

(* these are all boilerplate code. i should really be using polymorphic variants.*)
type pattern =
  | PInteger of int
  | PString of string
  | PBool of bool
  | PVariable of VarName.t
  | PRecord of DataName.t * (FieldName.t * pattern) list
  | PVariant of VarName.t * pattern list
  | PTuple of pattern list

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
  | Record of Loc.t * DataName.t * (FieldName.t * t) list
  | RecordIndex of Loc.t * t * FieldName.t
  | Tuple of Loc.t * t list
  | Variant of Loc.t * DataName.t * t list
  | Absurd of string * t

type toplevel =
  | Claim of Loc.t * VarName.t * A.ty
  | Def of Loc.t * VarName.t * t
  | VariantDef of Loc.t * DataName.t * (VarName.t * A.ty list) list
  | RecordDef of Loc.t * DataName.t * (FieldName.t * A.ty) list
  | AbilityDef of Loc.t * VarName.t * A.ty list
  | Expression of t

let rec pat_to_t = function
  | A.PInteger i -> PInteger i
  | A.PString s -> PString s
  | A.PBool b -> PBool b
  | A.PVariable v -> PVariable v
  | A.PTuple pats ->
      let pats = List.map pat_to_t pats in
      PTuple pats
  | A.PVariant (name, pats) ->
      let pats = List.map pat_to_t pats in
      PVariant (name, pats)
  | A.PRecord (name, pats) ->
      let pats = List.map (fun (n, p) -> (n, pat_to_t p)) pats in
      PRecord (name, pats)

let rec expr_to_t = function
  | A.Variable (loc, name) -> Variable (loc, name)
  | A.LitUnit loc -> LitUnit loc
  | A.LitInteger (loc, i) -> LitInteger (loc, i)
  | A.LitBool (loc, b) -> LitBool (loc, b)
  | A.LitFloat (loc, f) -> LitFloat (loc, f)
  | A.LitString (loc, s) -> LitString (loc, s)
  | A.Annotated (loc, e, ty) -> Annotated (loc, expr_to_t e, ty)
  | A.If (loc, p, pt, pf) -> If (loc, expr_to_t p, expr_to_t pt, expr_to_t pf)
  | A.Let (loc, pat, expr, body) ->
      Let (loc, pat_to_t pat, expr_to_t expr, expr_to_t body)
  | A.Fn (loc, names, body) -> Fn (loc, names, expr_to_t body)
  | A.Application (loc, operator, operands) ->
      Application (loc, expr_to_t operator, List.map expr_to_t operands)
  | A.Record (loc, name, body) ->
      let body = List.map (fun (n, e) -> (n, expr_to_t e)) body in
      Record (loc, name, body)
  | A.RecordIndex (loc, record, field) ->
      RecordIndex (loc, expr_to_t record, field)
  | A.Case (loc, expr, cases) ->
      let cases = List.map (fun (p, e) -> (pat_to_t p, expr_to_t e)) cases in
      Case (loc, expr_to_t expr, cases)
  | A.Tuple (loc, exprs) -> Tuple (loc, List.map expr_to_t exprs)
  | A.Sequence (loc, e1, e2) -> Sequence (loc, expr_to_t e1, expr_to_t e2)
  | A.Variant (loc, name, args) -> Variant (loc, name, List.map expr_to_t args)
  | A.LitTodo loc -> LitTodo loc
  | A.Absurd (s, e) -> Absurd (s, expr_to_t e)
  | A.Do _ | A.Handle _ -> assert false

let pp_list es f = es |> List.map f |> String.concat ", "

let rec pp_pattern = function
  | PInteger i -> Int.to_string i
  | PString s -> s
  | PVariable name -> VarName.to_string name
  | PTuple es -> Printf.sprintf "(%s)" (pp_list es pp_pattern)
  | PBool b -> Bool.to_string b
  | PVariant (name, es) ->
      Printf.sprintf "%s { %s }" (VarName.to_string name)
        (pp_list es pp_pattern)
  | PRecord (name, es) ->
      Printf.sprintf "%s {%s}" (DataName.to_string name)
      @@ pp_list es (fun (name, pattern) ->
             Printf.sprintf "%s: %s" (FieldName.to_string name)
               (pp_pattern pattern))

let rec pp_expression = function
  | LitTodo _loc -> "TODO"
  | LitUnit _loc -> "()"
  | LitBool (_loc, b) -> Bool.to_string b
  | LitInteger (_loc, i) -> Int.to_string i
  | LitFloat (_loc, f) -> Float.to_string f
  | LitString (_loc, s) -> s
  | Variable (_loc, v) -> VarName.to_string v
  | If (_loc, pred, tru, fals) ->
      Printf.sprintf "if %s then %s else %s" (pp_expression pred)
        (pp_expression tru) (pp_expression fals)
  | Application (_loc, rand, es) ->
      Printf.sprintf "%s (%s)" (pp_expression rand) (pp_list es pp_expression)
  | Let (_loc, var, value, body) ->
      Printf.sprintf "let %s = %s; %s" (pp_pattern var) (pp_expression value)
        (pp_expression body)
  | Fn (_loc, names, body) ->
      Printf.sprintf "fn (%s) %s"
        (pp_list names VarName.to_string)
        (pp_expression body)
  | Annotated (_loc, expr, ty) ->
      Printf.sprintf "(the %s %s)" (pp_expression expr) (A.pp_ty ty)
  | Sequence (_loc, a, b) ->
      Printf.sprintf "%s; %s;" (pp_expression a) (pp_expression b)
  | Case (_loc, expr, pes) ->
      (* pes - pattern, expression S *)
      let f (pat, expr) =
        Printf.sprintf "%s -> %s" (pp_pattern pat) (pp_expression expr)
      in
      Printf.sprintf "case %s { %s }" (pp_expression expr) (pp_list pes f)
  | Tuple (_loc, es) -> Printf.sprintf "(%s)" (pp_list es pp_expression)
  | Record (_loc, name, fes) ->
      (* fes - field, expression S *)
      let f (field, expr) =
        Printf.sprintf "%s: %s" (FieldName.to_string field) (pp_expression expr)
      in
      Printf.sprintf "%s {%s}" (DataName.to_string name) (pp_list fes f)
  | RecordIndex (_loc, expr, name) ->
      Printf.sprintf "%s.%s" (pp_expression expr) (FieldName.to_string name)
  | Variant (_loc, name, []) -> DataName.to_string name
  | Variant (_loc, name, args) ->
      Printf.sprintf "%s (%s)" (DataName.to_string name)
        (pp_list args pp_expression)
  | Absurd (s, e) -> Printf.sprintf "absurd (%s, %s)" s (pp_expression e)

let pp_toplevel = function
  | Claim (_loc, name, ty) ->
      Printf.sprintf "claim %s %s" (VarName.to_string name) (A.pp_ty ty)
  | Def (_loc, name, expr) ->
      (* TODO: add a special case for fn *)
      Printf.sprintf "def %s = %s" (VarName.to_string name) (pp_expression expr)
  | Expression expr -> pp_expression expr
  | VariantDef _ | RecordDef _ | AbilityDef _ -> "<def>"
(* for now *)

(* end of boilerplate *)

(* computations are represented as functions that take 
  a stack of functions, which alternate `k` and `h` continuations. 
  `k` continuations represent how we would normally return a value. 
   `h` coninutations represent how to handle an effect. *)

(* 
   for more details on this cps translation see:

   [1] https://dhil.net/research/papers/generalised_continuations-jfp2020.pdf
   [2] https://homepages.inf.ed.ac.uk/slindley/papers/handlers-cps.pdf
   [3] https://www.cs.uoregon.edu/research/summerschool/summer18/lectures/bauer_notes.pdf
   [4] https://raw.githubusercontent.com/matijapretnar/eff/master/docs/handlers-tutorial.pdf
*)

(* i what to use this as a marker for 
   computations that have been created. 
   what i *really* want to do is keep 
   metadata with the syntax, but oh well... *)
let d' =
  Lexing.{ pos_fname = "comp--00"; pos_lnum = -9; pos_bol = -8; pos_cnum = -7 }

let d = (d', d')

let is_cps_computation = function Fn (loc, _, _) -> loc = d | _ -> false

(* let d = Loc.dummy *)

let fresh_var =
  let state = ref 0 in
  fun id ->
    let s = Printf.sprintf "%s_%d" id !state in
    VarName.of_string s

let nil = LitUnit Loc.dummy

let cons =
  let e1 = fresh_var "e1" and e2 = fresh_var "e2" in
  Fn (d, [ e1; e2 ], Tuple (d, [ Variable (d, e1); Variable (d, e2) ]))

let rest =
  let pair = fresh_var "pair" and hd = fresh_var "hd" and tl = fresh_var "tl" in
  Fn
    ( d,
      [ pair ],
      Case
        ( d,
          Variable (d, pair),
          [ (PTuple [ PVariable hd; PVariable tl ], Variable (d, tl)) ] ) )

let first =
  let pair = fresh_var "pair" and hd = fresh_var "hd" and tl = fresh_var "tl" in
  Fn
    ( d,
      [ pair ],
      Case
        ( d,
          Variable (d, pair),
          [ (PTuple [ PVariable hd; PVariable tl ], Variable (d, hd)) ] ) )

let rec get_return_clause l =
  match l with
  | [] ->
      (* this should indicte a bug in the parser *)
      failwith "get_return_clause failed: no return clause in the handler"
  | A.Return (name, body) :: _ -> `Return (name, body)
  | _ :: rest -> get_return_clause rest

let rec get_operation_clauses l =
  match l with
  | [] -> []
  | A.Return _ :: rest -> get_operation_clauses rest
  | A.Operation (label, vars, kvar, body) :: rest ->
      `Operation (label, vars, kvar, body) :: get_operation_clauses rest

(* GENERAL RULE OF THUMB TO AVOID UNWEIEDLY BUGS:
   ALWAYS APPLY THE DESUGARING FUNCTION `g` BEFORE 
   CALLING `return` *)

(* implicitly lift *values* into *computations* 
   this is not available to the users just to keep 
   things simple *)
let rec return expr =
  let k = fresh_var "___k___" in
  (* i just put the the wierdest variable name i could think of *)
  let ks' = fresh_var "___ks'___" in
  let ks = fresh_var "___ks___" in
  match expr with
  (* | Let (l, p, body, expr) -> Let (l, p, body, expr) *)
  (* | Fn _ -> expr *)

  (* applications result in a computation that doesn't start with a function,
     but when eventually reduced, it produces a function that *should* be marked
     as a computation. *)
  | Application _ -> expr
  | expr when is_cps_computation expr -> expr
  | expr ->
      (* print_endline "yes"; *)
      let e =
        Fn
          ( d,
            [ ks ],
            Case
              ( d,
                Variable (d, ks),
                [
                  ( PTuple [ PVariable k; PVariable ks' ],
                    Application (d, Variable (d, k), [ expr; Variable (d, ks') ])
                  );
                ] ) )
      in
      (* print_endline (pp_expression e); *)
      e

(* rants: - applications, if .. then .. else .. are *computations*!  -
   in this setting, whenever we get a "should not be evaluated by me"
   error from the interpreter, we are probably using a computation as
   a value - turns out, returning from a handler clauses calls the
   continuation associated with that clause and calling the actual
   continuation does something *very* scary...  - also, a function
   body is a computation ... we might say we want to intercept every
   function call in the `g` function, but what about function
   application with variables?? (fucking first-class functions!)

       solutions: - we need to re-write the evaluator to explicity its
   closures, and the evaluator should be aware of this `g` transform -
   there is probably a bug with the handler case in this `g` transform

       also: - a handler returns a computation *)

(* what's left?  
   1. some computations are still treated like values -- this is unfair.
   2. function application needs to sequence its arguments. 
 
  ---------------------------------------

   bugs? 

   1. closure seem not to be working (pattern matching on arg as `ks`) 

*)

let rec g = function
  | A.LitBool (loc, b) -> LitBool (loc, b)
  | A.LitFloat (loc, f) -> LitFloat (loc, f)
  | A.LitInteger (loc, i) -> LitInteger (loc, i)
  | A.LitTodo loc -> LitTodo loc
  | A.LitString (loc, s) -> LitString (loc, s)
  | A.LitUnit loc -> LitUnit loc
  | A.Variable (loc, v) -> Variable (loc, v)
  | A.Variant (loc, name, args) -> Variant (loc, name, List.map g args)
  | A.Tuple (loc, elems) -> Tuple (loc, List.map g elems)
  | A.Record (loc, name, args) ->
      let args = List.map (fun (n, e) -> (n, g e)) args in
      Record (loc, name, args)
  (* btw this is also a computation *)
  | A.RecordIndex (loc, expr, name) -> RecordIndex (loc, g expr, name)
  | A.Annotated (loc, expr, ty) -> Annotated (loc, g expr, ty)
  (* this is also a computation (can easily be translated into a let) *)
  | A.Sequence (loc, a, b) -> Sequence (loc, g a, g b)
  | A.Fn (loc, vars, body) -> Fn (loc, vars, return (g body))
  | A.Do (_loc, label, args) ->
      let args = Tuple (d, List.map g args) in
      let ks = fresh_var "ks" and k = fresh_var "k" and h = fresh_var "h" in
      let tag = LitString (d, VarName.to_string label) in
      let x, ks2 = (fresh_var "x", fresh_var "ks") in
      let ks' = fresh_var "ks'" in
      Fn
        ( d,
          [ ks ],
          Let
            ( d,
              PTuple [ PVariable k; PTuple [ PVariable h; PVariable ks' ] ],
              Variable (d, ks),
              Application
                ( d,
                  Variable (d, h),
                  [
                    Tuple
                      ( d,
                        [
                          tag;
                          args;
                          Fn
                            ( d,
                              [ x ],
                              Fn
                                ( d,
                                  [ ks2 ],
                                  Application
                                    ( d,
                                      Variable (d, k),
                                      [
                                        Variable (d, x);
                                        Application
                                          ( d,
                                            cons,
                                            [
                                              Variable (d, h); Variable (d, ks2);
                                            ] );
                                      ] ) ) );
                        ] );
                    Variable (d, ks');
                  ] ) ) )
  | A.Let (_loc, pat, expr, body) ->
      let get_variable = function
        | A.PVariable x -> x
        | _ ->
            failwith
              "can only bind a variable with a let pattern before g transform"
      in
      let x = get_variable pat in
      let ks, k, ks', f, ks'' =
        ( fresh_var "ks",
          fresh_var "k",
          fresh_var "ks'",
          fresh_var "f",
          fresh_var "ks''" )
      in
      let e =
        Fn
          ( d,
            [ ks ],
            Let
              ( d,
                PTuple [ PVariable k; PVariable ks' ],
                Variable (d, ks),
                Let
                  ( d,
                    PVariable f,
                    Fn
                      ( d,
                        [ x; ks'' ],
                        Application
                          ( d,
                            return (g body),
                            [
                              Application
                                ( d,
                                  cons,
                                  [ Variable (d, k); Variable (d, ks'') ] );
                            ] ) ),
                    Application
                      ( d,
                        return (g expr),
                        [
                          Application
                            (d, cons, [ Variable (d, f); Variable (d, ks') ]);
                        ] ) ) ) )
      in
      (* print_string "the let: ";
         print_endline (pp_expression e);
         print_endline "after let" ; *)
      e
  | A.Handle (_loc, expr, clauses) ->
      let ks, k1, z, k2 =
        (fresh_var "ks", fresh_var "k1", fresh_var "z", fresh_var "k2")
      in
      let ret =
        let (`Return (name, body)) = get_return_clause clauses in
        let h, ks' = (fresh_var "h", fresh_var "ks'") in
        (* print_endline "here";
           print_endline (pp_expression (g body)); *)
        Fn
          ( d,
            [ name; ks ],
            Let
              ( d,
                PTuple [ PVariable h; PVariable ks' ],
                Variable (d, ks),
                Application (d, return (g body), [ Variable (d, ks') ]) ) )
      in
      let g_clause ks clause =
        let (`Operation (label, vars, kvar, body)) = clause in
        let label = VarName.to_string label in
        let pvars = vars |> List.map (fun var -> PVariable var) in
        let pat = PTuple [ PString label; PTuple pvars; PVariable kvar ] in

        (* print_endline "here we go";
           print_endline (pp_expression (return @@ g body));
           there are some subtleties here
           we need to return only when we know body is a value
           before the transform *)
        let body = Application (d, return (g body), [ Variable (d, ks) ]) in
        (pat, body)
      in
      let cases = clauses |> get_operation_clauses |> List.map (g_clause k1) in
      let foward (label, arg, kvar) ks =
        let k', h', ks', f, x, ks'' =
          ( fresh_var "k'",
            fresh_var "h'",
            fresh_var "ks'",
            fresh_var "f",
            fresh_var "x",
            fresh_var "ks''" )
        in
        Let
          ( d,
            PTuple [ PVariable k'; PTuple [ PVariable h'; PVariable ks' ] ],
            Variable (d, ks),
            Let
              ( d,
                PVariable f,
                Fn
                  ( d,
                    [ x ],
                    Fn
                      ( d,
                        [ ks'' ],
                        Application
                          ( d,
                            Application
                              (d, Variable (d, kvar), [ Variable (d, x) ]),
                            [
                              Application
                                ( d,
                                  cons,
                                  [
                                    Variable (d, k');
                                    Application
                                      ( d,
                                        cons,
                                        [ Variable (d, h'); Variable (d, ks'') ]
                                      );
                                  ] );
                            ] ) ) ),
                Application
                  ( d,
                    Variable (d, h'),
                    [
                      Tuple (d, [ label; arg; Variable (d, f) ]);
                      Variable (d, ks');
                    ] ) ) )
      in
      let cases = cases in
      let op_clauses =
        let label, args, kvar =
          (fresh_var "label", fresh_var "args", fresh_var "kvar")
        in
        Fn
          ( d,
            [ z; k1 ],
            Case
              ( d,
                Variable (d, z),
                cases
                @ [
                    ( PTuple [ PVariable label; PVariable args; PVariable kvar ],
                      foward (Variable (d, label), Variable (d, args), kvar) k1
                    );
                  ] ) )
      in
      Fn
        ( d,
          [ k2 ],
          Application
            ( d,
              g expr,
              [
                Application
                  ( d,
                    cons,
                    [
                      ret;
                      Application (d, cons, [ op_clauses; Variable (d, k2) ]);
                    ] );
              ] ) )
  (* these are actually computation. idk for now *)
  | A.Case (loc, expr, clauses) ->
      let clauses =
        List.map (fun (pat, expr) -> (pat_to_t pat, g expr)) clauses
      in
      Case (loc, g expr, clauses)
  | A.If (_loc, p, pt, pf) ->
      let ks, k, ks', f =
        (fresh_var "ks", fresh_var "k", fresh_var "ks'", fresh_var "f")
      in
      let x, ks'' = (fresh_var "x", fresh_var "ks''") in
      (* If (loc, g p, g pt, g pf)
         Application (d, return (g body),
         [Application (d, cons, [Variable (d, k); Variable (d, ks'')])]
      *)
      let e =
        Fn
          ( d,
            [ ks ],
            Let
              ( d,
                PTuple [ PVariable k; PVariable ks' ],
                Variable (d, ks),
                Let
                  ( d,
                    PVariable f,
                    Fn
                      ( d,
                        [ x; ks'' ],
                        Application
                          ( d,
                            If (d, Variable (d, x), return (g pt), return (g pf)),
                            [
                              Application
                                ( d,
                                  cons,
                                  [ Variable (d, k); Variable (d, ks'') ] );
                            ] ) ),
                    Application
                      ( d,
                        return (g p),
                        [
                          Application
                            (d, cons, [ Variable (d, f); Variable (d, ks') ]);
                        ] ) ) ) )
      in
      e
  | A.Application (_loc, f, args) ->
      let ks, k, ks', fn =
        (fresh_var "ks", fresh_var "k", fresh_var "ks'", fresh_var "fn")
      in
      let x, ks'' = (fresh_var "x", fresh_var "ks''") in
      let args' = List.map expr_to_t args in
      let e =
        Fn
          ( d,
            [ ks ],
            Let
              ( d,
                PTuple [ PVariable k; PVariable ks' ],
                Variable (d, ks),
                Let
                  ( d,
                    PVariable fn,
                    Fn
                      ( d,
                        [ x; ks'' ],
                        Application
                          ( d,
                            Application (d, Variable (d, x), args'),
                            [
                              Application
                                ( d,
                                  cons,
                                  [ Variable (d, k); Variable (d, ks'') ] );
                            ] ) ),
                    Application
                      ( d,
                        return (g f),
                        [
                          Application
                            (d, cons, [ Variable (d, fn); Variable (d, ks') ]);
                        ] ) ) ) )
      in
      (* let e = Application (loc, expr_to_t f, args') in *)
      (* print_endline "application";
         print_endline @@ pp_expression e; *)
      e
  | A.Absurd (s, e) -> Absurd (s, expr_to_t e)

(* this should always be the body of a plain expression body of a handler clause *)
(* A.Plain x what if i transform x, just like the plain case here? *)
(* let ks = fresh_var "ks" in
   A.Fn (d,
      [ks],
      A.Application
        (d,
         A.Application (d, first, [A.Variable (d, ks)]),
         [e; A.Application (d, rest, [A.Variable (d, ks)])])) *)
(* A.Plain e *)

(*
   Tuple (d, vars)

   P =  label:string, (args ...), k
*)

let const =
  let x, ks = (fresh_var "x", fresh_var "ks") in
  Fn (d, [ x; ks ], Variable (d, x))

let handler =
  let comp, ks = (fresh_var "comp", fresh_var "ks") in
  let l, idk1, idk2 = (fresh_var "l", fresh_var "idk1", fresh_var "idk2") in
  Fn
    ( d,
      [ comp; ks ],
      Let
        ( d,
          PTuple [ PVariable l; PVariable idk1; PVariable idk2 ],
          Variable (d, comp),
          Absurd ("Unhandled effect", Variable (d, l)) ) )

let handler_l = Application (d, cons, [ handler; nil ])

let handlers = Application (d, cons, [ const; handler_l ])

let handle_comp computation =
  (* let gs = pp_expression (g computation) in
     let after_gs = pp_expression @@ ret (g computation) computation in
     print_endline "before g "; print_endline gs;
     print_endline "after g"; print_endline after_gs; *)
  Application (d, return (g computation), [ handlers ])

let desugar_toplevel l =
  let f = function
    | A.Def (loc, name, expr) -> Def (loc, name, handle_comp expr)
    | A.Expression e -> Expression (handle_comp e)
    | A.Claim (loc, name, ty) -> Claim (loc, name, ty)
    | A.VariantDef (loc, name, body) -> VariantDef (loc, name, body)
    | A.AbilityDef (loc, name, args) -> AbilityDef (loc, name, args)
    | A.RecordDef (loc, name, body) -> RecordDef (loc, name, body)
  in
  List.map f l

(* the major problem now is that returning from a handler clause 
   without calling the continuation doesn't work *)

(* okay, i know whats wrong. 
   the body expression on the clause is expected to be a computation, but 
   in the `sc` transform we leave expressions just the way they are. 
   for this to work, we have to lift expressions that are not computations 
   using the plain constructor. 

   this is basically selective lifting. 
   i.e if we have a `Let` we wouldn't lift it
       but if we have a variable, or an integer, 
       we would lift it using `Plain`

   a variable *cannot* be a computation; lambdas are the only syntactic 
   construct that can generate a computation (its arguments are values), 
   hence their elimination form must be handled with care.

   i'm certain this would work.

*)
