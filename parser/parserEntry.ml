open Lexing
(* open Syntax for using Loc.t maybe? *)
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Grammar.MenhirInterpreter

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


