

exception RuntimeError of string

let runtime s = raise @@ RuntimeError s




