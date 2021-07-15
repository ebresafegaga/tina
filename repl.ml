open Syntax
open Runtime
open Backend    

module P = Parser.ParserEntry

(* THE TINA REPL *)

let state = object
  val loaded_files : string list ref = ref []
  val dump : bool ref = ref false

  method get_files () = !loaded_files
  method add_file file = loaded_files := !loaded_files @ [file]
                                           
  method get_dump () = !dump
  method start_dump () = dump := true
  method end_dump () = dump := false

  method clear () =
    loaded_files := [];
    dump := false 
end

let prompt = "tina*> "
let print_to_repl = print_endline
let print_prompt () = print_string prompt
let read_line () = read_line () |> String.trim
let print_error msg = print_to_repl msg
let print_list lst = lst |> List.iter print_to_repl

let is_command s = s.[0] = ':'

let eval lexbuf =
  lexbuf
  |> P.parse
  |> DesugarEffect.desugar_toplevel 
  |> List.map (fun expr ->
      if state#get_dump () then
        (expr
         |> DesugarEffect.pp_toplevel
         |> print_endline; expr)
      else expr)
  |> Eval2.process_toplevel
  |> String.concat "\n"
  |> print_to_repl

let rec process_command input =
  let commands = String.split_on_char ' ' input in
  (* print_int (List.length commands); *)
  (* print_list commands; *)
  match commands with
  | [] ->  repl ()
  | ":load" :: args ->
    process_load args;
    repl ()
  | ":list" :: _args ->
    print_list @@ state#get_files ();
    repl ()
  | ":reload" :: _args ->
    let files = state#get_files () in
    state#clear ();
    process_load files;
    repl ()
  | ":clear" :: _args ->
    state#clear (); repl ()
  | ":quit" :: _args ->
    print_to_repl "Bye Bye!";
    exit 0;
  | ":dump" :: "start" :: _args ->
    print_to_repl "dumping started ...\n";
    state#start_dump (); repl ()
  | ":dump" :: "end" :: _args ->
    print_to_repl "dumping ending ...\n";
    state#end_dump (); repl ()
  | ":desugar" :: "data" :: files -> process_desugar_data files; repl ()
  | ":desugar" :: "case" :: files -> process_desugar_case files; repl ()
  | ":compile" :: "js" :: [tina; js] -> process_js_compile tina js; repl ()
  | invalid :: _args ->
    let msg = Printf.sprintf "invalid command %s" invalid in
    print_error msg;
    repl ()

and process_js_compile tina js =
  let process_data channel =
    channel
    |> Lexing.from_channel
    |> P.parse
    |> DesugarData.handle_toplevel
    |> DesugarCase.handle_toplevel
    |> Js.gen_toplevel
    |> String.concat "\n"
  in
  let tina = open_in tina in 
  let file = open_out js in
  Printf.fprintf file "%s" (process_data tina)
    
and process_load = function
  | [] -> ()
  | file :: files ->
    (* Printf.printf "about to open file %s \n" file;*)
    match open_in file with
    | channel ->
      state#add_file file;
      (try process_file channel with
         Errors.RuntimeError m -> print_error m);
      process_load files
    | exception Sys_error msg ->
      print_error msg;
      process_load files

and process_desugar_data =
  let process_data channel =
    channel
    |> Lexing.from_channel
    |> P.parse
    |> DesugarData.handle_toplevel
    (* |> DesugarCase.handle_toplevel *)
    |> List.map DesugarData.pp_toplevel
    |> String.concat "\n"
    |> print_to_repl
  in
  function 
  | [] -> ()
  | file :: files ->
    match open_in file with
    | channel ->
      (try process_data channel with
         Errors.RuntimeError m -> print_error m);
      process_desugar_data files
    | exception Sys_error msg ->
      print_error msg;
      process_desugar_data files

and process_desugar_case =
  let process_data channel =
    channel
    |> Lexing.from_channel
    |> P.parse
    |> DesugarData.handle_toplevel
    |> DesugarCase.handle_toplevel
    |> List.map DesugarCase.pp_toplevel
    |> String.concat "\n"
    |> print_to_repl
  in
  function 
  | [] -> ()
  | file :: files ->
    match open_in file with
    | channel ->
      (try process_data channel with
         Errors.RuntimeError m -> print_error m);
      process_desugar_data files
    | exception Sys_error msg ->
      print_error msg;
      process_desugar_data files

and process_file channel =
  channel
  |> Lexing.from_channel
  |> eval

and process_line input =
  let _files = state#get_files () in
  input
  |> Lexing.from_string
  |> eval

and repl () =
  print_prompt ();
  let line = read_line () in
  match line with
  | "" -> repl ()
  | _ ->
    (if is_command line then
       process_command line
     else
       try process_line line with
         Errors.RuntimeError m -> print_error m);
    repl ()
      
let run () =
  print_to_repl "Welcome to Tina: programming with typed algebraic effects \n";
  repl ()

