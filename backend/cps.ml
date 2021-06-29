open Syntax

module A = Ast

let g _k = function
  | A.Variable (_loc, _v) -> failwith ""
  | _ -> failwith ""
