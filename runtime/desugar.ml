open Syntax
open Naming

module A = Ast

let d = Loc.dummy

let fresh_var =
  let state = ref 0 in
  fun id ->
    let s = Printf.sprintf "%s_%d" id !state in
    VarName.of_string s

let nil = A.LitUnit Loc.dummy

let cons =
  let e1  = fresh_var "e1"
  and e2 = fresh_var "e2" in
  A.Fn (d,
        [e1; e2],
        A.Tuple (d,
                 [A.Variable (d, e1);
                  A.Variable (d, e2)]))

let rest =
  let pair  = fresh_var "pair"
  and hd = fresh_var "hd"
  and tl = fresh_var "tl" in
  A.Fn (d,
        [pair],
        A.Case (d, A.Variable (d, pair),
              [A.PTuple [A.PVariable hd; A.PVariable tl],
               A.Variable (d, tl)]))

let first =
  let pair  = fresh_var "pair"
  and hd = fresh_var "hd"
  and tl = fresh_var "tl" in
  A.Fn (d,
        [pair],
        A.Case (d, A.Variable (d, pair),
              [A.PTuple [A.PVariable hd; A.PVariable tl],
               A.Variable (d, hd)]))       

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

(* rants:
   - applications, if .. then .. else .. are *computations*!
   - in this setting, whenever we get a "should not be evaluated by me" error 
     from the interpreter, we are probably using a computation as a value
   - turns out, returning from a handler clauses calls the continuation 
     associated with that clause and calling the actual continuation does 
     something *very* scary...
   - also, a function body is a computation ... we might say we want to 
     intercept every function call in the `g` function, but what about 
     function application with variables?? (fucking first-class functions!)

   solutions: 
   - we need to re-write the evaluator to explicity its closures, and 
     the evaluator should be aware of this `g` transform
   - there is probably a bug with the handler case in this `g` transform
*)
let rec g term =
  (*  Printf.printf "%s" (A.pp_expression term); *)
  match term with
  (* | A.Fn (loc, vars, body) -> A.Fn (loc, vars, g body)*)
  | A.Plain e ->
    (* print_endline "i got here";*)
    let ks = fresh_var "ks" in
    A.Fn (d,
          [ks],
          A.Application
            (d,
             A.Application (d, first, [A.Variable (d, ks)]),
             [g e; A.Application (d, rest, [A.Variable (d, ks)])]))

  | A.Do (_loc, label, args) ->
    let args = A.Tuple (d, List.map g args) in
    let ks = fresh_var "ks"
    and k = fresh_var "k"
    and h = fresh_var "h" in
    let tag = A.LitString (d, VarName.to_string label) in
    let x, ks2 = fresh_var "x", fresh_var "ks" in
    A.Fn
      (d,
       [ks],
       A.Let (d, A.PVariable k, A.Application (d, first , [A.Variable (d, ks)]),
              A.Let (d, A.PVariable h, A.Application (d, first, [A.Application (d, rest, [A.Variable (d, ks)])]),
                     A.Application (d, A.Variable (d, h), [A.Tuple (d, [tag; args;
                                                                        A.Fn
                                                                          (d,
                                                                           [x], 
                                                                           A.Fn (d, [ks2], A.Application
                                                                                   (d, A.Variable (d, k), 
                                                                                    [A.Variable (d, x);
                                                                                     A.Application (d, cons,
                                                                                                    [A.Variable (d, h);
                                                                                                     A.Variable (d, ks2)])])))]);
                                                           A.Variable (d, ks)]))))
  | A.Let (_loc, pat, expr, body) ->
    let get_variable = function
      | A.PVariable x -> x
      | _ -> failwith "can only bind a variable with a let pattern before g transform"
    in
    let x = get_variable pat in
    let ks, k, ks', f, ks'' =
      fresh_var "ks", fresh_var "k",
      fresh_var "ks'", fresh_var "f",
      fresh_var "ks''"
    in
    A.Fn
      (d,
       [ks],
       A.Let (d, A.PTuple [A.PVariable k; A.PVariable ks'], A.Variable (d, ks),
              A.Let (d, A.PVariable f, A.Fn (d, [x; ks''],
                                             A.Application (d, g body, [A.Application (d, cons, [A.Variable (d, k);
                                                                                                 A.Variable (d, ks'')])])),
                     A.Application (d, g expr, [A.Application (d, cons, [A.Variable (d, f);
                                                                         A.Variable (d, ks')])]))))       
  | A.Handle (_loc, expr, clauses) ->
    let ks, k1, z, k2 = fresh_var "ks", fresh_var "k1", fresh_var "z", fresh_var "k2" in
    let ret =
      let `Return (name, body) = get_return_clause clauses in
      let h, ks' = fresh_var "h", fresh_var "ks'" in
      A.Fn
        (d,
         [name; ks],
         A.Let(d, A.PTuple [A.PVariable h; A.PVariable ks'], A.Variable (d, ks),
               A.Application (d, g body, [A.Variable (d, ks')])))
    in
    let g_clause ks clause =
      let `Operation (label, vars, kvar, body) = clause in
      let label = VarName.to_string label in
      let pvars = vars |> List.map (fun var -> A.PVariable var) in
      let pat = A.PTuple [A.PString label; A.PTuple pvars; A.PVariable kvar] in
      let body = A.Application (d, g body, [A.Variable (d, ks)]) in
      (pat, body)
    in
    let cases =
      clauses
      |> get_operation_clauses
      |> List.map (g_clause k1)
    in
    let foward (label, arg, kvar) ks =
      let k', h', ks', f, x, ks'' =
        fresh_var "k'", fresh_var "h'",
        fresh_var "ks'", fresh_var "f",
        fresh_var "x", fresh_var "ks''"
      in
      A.Let (d, A.PTuple [A.PVariable k'; A.PTuple [A.PVariable h'; A.PVariable ks']], A.Variable (d, ks),
             A.Let (d, A.PVariable f,
                    A.Fn (d, [x],
                          A.Fn (d, [ks''], A.Application (d, A.Application (d, A.Variable (d, kvar),
                                         [A.Variable (d, x)]),
                                          [A.Application (d, cons, [A.Variable (d, k');
                                                                   A.Application (d, cons,
                                                                     [A.Variable(d, h'); A.Variable (d, ks'')])])]))),
                    A.Application (d, A.Variable (d, h'), [ A.Tuple (d, [label; arg; A.Variable (d, f)]); A.Variable (d, ks')])))
    in       
    let cases = cases in
    let op_clauses =
      let label, args, kvar =
        fresh_var "label", fresh_var "args",
        fresh_var "kvar"
      in
      A.Fn(d, [z; k1],
           A.Case (d, A.Variable (d, z), cases @
                                         [A.PTuple [A.PVariable label; A.PVariable args; A.PVariable kvar],
                                          foward (A.Variable (d, label), A.Variable (d, args), kvar) k1]))
    in
    A.Fn (d, [k2], A.Application (d, g expr, [A.Application (d, cons, [ret; A.Application
                                                                         (d, cons, [op_clauses; A.Variable (d, k2)])])]))
  | e ->
    (* this should always be the body of a plain expression body of a handler clause *)
    (* A.Plain x what if i transform x, just like the plain case here? *)
    (*  let ks = fresh_var "ks" in
        A.Fn (d,
           [ks],
           A.Application
             (d,
              A.Application (d, first, [A.Variable (d, ks)]),
              [e; A.Application (d, rest, [A.Variable (d, ks)])])) *)
    A.Plain e


    (* 
   Tuple (d, vars)

   P =  label:string, (args ...), k
*)

    let const =
      let x, ks = fresh_var "x", fresh_var "ks" in
      A.Fn (d, [x; ks], A.Variable (d, x))

let handler =
  let comp, ks = fresh_var "comp", fresh_var "ks" in
  A.Fn (d, [comp; ks],
        ( A.LitTodo (d) ))

let handler_l = A.Application (d, cons, [handler; nil])

let handlers = A.Application (d, cons, [const; handler_l])
  
let handle_comp computation =
  A.Application (d, g computation, [handlers])


let desugar_toplevel l =
  let f = function
    | A.Def (loc, name, expr) -> A.Def (loc, name, handle_comp expr) (* this is actually wrong, the value gotten from evaluating handle_comp should be bound to name -- i've corrected it, it was `g expr` before *)
    | A.Expression (e) -> A.Expression (handle_comp e)
    | any -> any
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
   hence their elimination form
   must be handled with care.

   i'm certain this would work.

*)
