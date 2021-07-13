
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

module List = struct
  include List

  let rec from_exclusive n m =
    if n < m then
      n :: from_exclusive (n+1) m
    else
      []
end


let (>>) f g x = x |> f |> g

