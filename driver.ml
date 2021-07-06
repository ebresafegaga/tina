open Syntax
open Runtime

module P = Parser.ParserEntry

let () =
  Sys.argv.(1)
  |> open_in
  |> Lexing.from_channel
  |> P.parse
  |> P.sc_toplevel
  |> DesugarEffect.desugar_toplevel
  (* |> List.map (fun expr ->
      expr
      |> Ast.pp_toplevel
      |> print_endline;
      expr) dump ast *)
  |> Eval2.process_toplevel
  |> String.concat "\n"
  |> Printf.printf "%s\n"
