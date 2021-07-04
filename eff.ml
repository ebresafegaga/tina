
(*
effect Add : int * int -> int 

let comp () =
  let a = perform @@ Add (5, 6) in
  let b = perform @@ Add (2, 2) in
  a + b

let () =
  let result =
    match comp () with
    | v -> v
    | effect (Add (a, b)) k -> continue k (a+b)
  in
  print_int result
*)
