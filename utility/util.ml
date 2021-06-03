
module Result = struct 
    include Result

    let (let*) = bind
    let ( let+ ) = map

    let rec sequenceA xs = 
        match xs with 
        | [] -> Ok []
        | x :: xs -> 
            let* x = x in 
            let* xs = sequenceA xs in 
            Ok (x :: xs)
    
end