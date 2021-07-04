open Lexing

(* open Syntax for using Loc.t maybe? *)
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Grammar.MenhirInterpreter
module A = Syntax.Ast

type message = string
(* TODO: replace Lexing.position with Loc.t from 
   Syntax  *)
type parse_error =
  | LexingError of string * Lexing.position
  | SyntaxError of message option * Lexing.position * Lexing.position

exception Error of parse_error

(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env]. *)
let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ -> assert false

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint. *)
let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) -> I.number s
  | None ->
    (* The parser is in its initial state. The incremental API
       currently lacks a way of finding out the number of the initial
       state. It is usually 0, so we return 0. *)
    0

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)
let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)

(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. *)
let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) ->
    show text (pos1, pos2)
  | None ->
    (* The index is out of range. This should not happen if [$i]
       keywords are correctly inside the syntax error message
       database. The integer [i] should always be a valid offset
       into the known suffix of the stack. *)
    "???"

(* [succeed v] is invoked when the parser has succeeded and produced a
   semantic value [v]. *)
let succeed v = v

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)
let fail text buffer checkpoint =
  (* the format for this string: File \%s\, line %d, characters %d-%d:\n *)
  let location = L.range (E.last buffer) in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  let message = E.expand (get text checkpoint) message in
  let msg = Printf.sprintf "%s at %s" message location in
  failwith msg (* for now just failwith the message *)
  
let parse lexbuf =
  let text = lexbuf.lex_buffer |> Bytes.to_string in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read_token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Grammar.Incremental.toplevel lexbuf.lex_curr_p in
  I.loop_handle succeed (fail text buffer) supplier checkpoint

let rec sc =
  let plain x = A.Plain x in
  function
  | A.Let (loc, name, expr, body) ->
    A.Let (loc, name, sc expr, sc body)
  | A.Do (loc, name, args) ->
    A.Do (loc, name, args)
  | A.Handle (loc, expr, clauses) ->
    A.Handle (loc, sc expr, List.map sc_clauses clauses)
  | A.Plain e -> A.Plain e (* hmm *)
  | A.LitTodo _ as t -> plain t
  | A.LitUnit _ as u -> plain u
  | A.LitBool _ as b -> plain b
  | A.LitInteger _ as i -> plain i
  | A.LitFloat _ as f -> plain f
  | A.LitString _ as s -> plain s
  | A.Variable  _ as v -> plain v
  | A.If (loc, p, pt, pf) ->
    let iff = A.If (loc, sc p, sc pt, sc pf) in
    plain iff
  | A.Application (loc, rator, rand) ->
    let app = A.Application (loc, sc rator, List.map sc rand) in
    plain app
  | A.Fn (loc, args, body) ->
    let fn = A.Fn (loc, args, sc body) in
    plain fn
  | A.Annotated (loc, e, ty) ->
    let ann = A.Annotated (loc, sc e, ty) in
    plain ann
  | A.Sequence (loc, a, b) ->
    let seq = A.Sequence (loc, sc a, sc b) in
    plain seq
  | A.Case (loc, expr, clauses) ->
    let cases = A.Case (loc, sc expr, List.map (fun (p, e) -> (p, sc e)) clauses) in
    plain cases
  | A.Record (loc, name, body) ->
    let record = A.Record (loc, name, List.map (fun (n, e) -> (n, sc e)) body) in
    plain record
  | A.RecordIndex (loc, e, name) ->
    let record_index = A.RecordIndex (loc, sc e, name) in
    plain record_index
  | A.Tuple (loc, es) ->
    let tuple = A.Tuple (loc, List.map sc es) in
    plain tuple
  
and sc_clauses = function
  | A.Return (name, expr) -> A.Return (name, sc expr)
  | A.Operation (name, args, kvar, expr) -> A.Operation (name, args, kvar, return expr)

and return expr =
  let open Syntax in
  let open Naming in
  let d = Loc.dummy in
  let x = VarName.of_string "___x" in
  let ks = VarName.of_string "___ks" in
  match expr with
  | A.Let (l, p, body, expr) -> A.Let (l, p, return body, return expr)
  | A.Do _ | A.Handle _ | A.Application _ -> expr
  | _ -> A.Fn (d, [ks],
        A.Let (d, A.PVariable x, expr, A.Variable (d, x)))


let sc_toplevel l =
  let f = function
    | A.Expression e -> A.Expression (sc e)
    | A.Def (loc, name, e) -> A.Def (loc, name, sc e)
    | x -> x
  in
  List.map f l

let pp_token =
  let module G = Grammar in
  function
  | G.INT i -> Int.to_string i
  | G.FLOAT f -> Float.to_string f
  | G.ID id -> Printf.sprintf "ID %s" id
  | G.STRING s -> Printf.sprintf "STRING %s" s
  | G.TRUE -> "TRUE" | G.FALSE -> "FALSE"
  | G.LBRACE -> "LBRACE" | G.RBRACE -> "RBRACE"
  | G.LPAREN -> "LPAREN" | G.RPAREN -> "RPAREN"
  | G.LBRACK -> "LBRACK" | G.RBRACK -> "RBRACK"
  | G.COMMA -> "COMMA" | G.COLON -> "COLON" | G.SEMICOLON -> "SEMICOLON"
  | G.EQUALS -> "EQUALS" | G.BAR -> "BAR" | G.CLAIM -> "CLAIM"
  | G.DEF -> "DEF" | G.DATA -> "DATATYPE" | G.CASE -> "CASE"
  | G.ABILITY -> "ABILITY" (* to use *)
  | G.ARROW -> "ARROW" | G.LET -> "LET" | G.FN -> "FN"
  | G.MUT -> "MUT" (* to use *)
  | G.END -> "END" (* to use *)
  | G.COLONEQUALS -> "COLONEQUALS" (* to use *)
  | G.IF -> "IF" | G.THEN -> "THEN"
  | G.ELSE -> "ELSE" | G.PLUS -> "PLUS"
  | G.STAR -> "STAR" | G.MINUS -> "MINUS"
  | G.DIV -> "DIV" | G.GT -> "GT"
  | G.LT -> "LT" | G.GTEQUALS -> "GTEQUALS"
  | G.LTEQUALS -> "LTEQUALS" | G.TY_NAT -> "TYNAT"
  | G.TY_INT -> "TYINT" | G.TY_FLOAT -> "TYFLOAT"
  | G.TY_STRING -> "TYSTRING" | G.TK_TODO -> "TKTODO"
  | G.THE -> "THE" | G.DOT -> "DOT" | G.EOF -> "EOF"

(* i don't apply sc to the arguments of do and the expression body of handler clauses *)
