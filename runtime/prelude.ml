


let plus = function
    | [Value.VInteger i; Value.VInteger i'] -> Ok (Value.VInteger (i + i'))
    | _ -> Error "Invalid argumnets"