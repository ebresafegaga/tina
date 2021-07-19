open Syntax
open Runtime
open Backend
open Typing
module P = Parsing.ParserEntry

let eval source =
  let syntax = source |> Lexing.from_string |> P.parse in
  (* let ty =
    syntax |> Typecheck.handle_toplevel |> Ctx.pp_ctx |> String.concat "\n"
     in*)
  let term = syntax (* |> DesugarEffect.handle_toplevel*) |> Eval.process_toplevel |> String.concat "\n" in
  Printf.sprintf "%s" term

let eval source =
  match eval source with
  | result -> Format.fprintf !Settings.output_formatter "%s@." result
  | exception Errors.RuntimeError msg ->
      Format.fprintf !Settings.error_formatter "%s@." msg

let execute_source = eval

let load_source = eval

let compile_js source =
  let process src =
    src |> Lexing.from_string |> P.parse |> DesugarEffect.handle_toplevel
    |> DesugarData.handle_toplevel |> DesugarCase.handle_toplevel
    |> KNormal.handle_toplevel |> Js.handle_toplevel |> List.map Js.gen_toplevel
    |> String.concat "\n"
  in
  let js_code = process source in
  Format.fprintf !Settings.error_formatter "%s@." js_code

let complie_llvm = ()
