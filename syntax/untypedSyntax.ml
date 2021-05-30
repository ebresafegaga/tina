open Utils

type pattern = 
    | Var 

type expresion = 
    | Void 
    | Variable
    | Constant
    | If 
    | Application
    | Let 
    | List 
    | Fn 
    | Annotated 
    | Sequence 
    | Match 
    | Record 
    | TODO
    
type toplevel = 
    | Claim 
    | Def