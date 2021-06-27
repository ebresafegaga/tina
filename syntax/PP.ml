

let pp_range ppf loc = 
    Format.printf ppf ""

let pp_expression ppf expr = 
    match expr with 
    | Ast.LitUnit (loc) -> Format.printf ppf "(unit)"
    | Ast.Variable (loc, name) -> Format.printf ppf "Variable"

let pp_toplevel ppf top = 
    match top with 
    | Ast.Claim (loc, name, ty) -> Format.printf ppf ""
    | Ast.Def (loc, name, body) -> Format.printf ppf ""
    | Ast.Expression e -> pp_expression ppf e


