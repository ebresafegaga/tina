open Js_of_ocaml

let js_formatter _format echo =
  let buffer = ref "" in
  let out s p n = buffer := !buffer ^ String.sub s p n in
  let flush () =
    (Js.Unsafe.fun_call echo [| Js.Unsafe.inject (Js.string !buffer) |] : unit);
    buffer := ""
  in
  Format.make_formatter out flush

(* Export the interface to Javascript. *)
let () =
  Js.export "jstina"
    (object%js
       method initialize echo =
         Settings.output_formatter := js_formatter "[;#00a8ff;#192a56]" echo;
         Settings.error_formatter := js_formatter "[b;#e84118;#192a56]" echo;
         Format.fprintf !Settings.output_formatter "Tina %s@." Settings.version

       method executeSource source =
         Shell.execute_source (Js.to_string source)

       method loadSource source = Shell.execute_source (Js.to_string source)

       method compileJS source = Shell.compile_js (Js.to_string source)
    end)
