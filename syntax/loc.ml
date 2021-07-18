
(*
type position = {
  pos_fname : string; <-- the file name
  pos_lnum : int;     <-- the line number 
  pos_bol : int;      <-- offset from the begining of the file (in chars, i.e no of chars)
  pos_cnum : int;
}

pos_cnum is the offset of the position (number of characters between the beginning of the lexbuf and the position).
 The difference between pos_cnum and pos_bol is the character offset within the line (i.e. the column number, 
assuming each character is one column wide).

*)

type t = Lexing.position * Lexing.position

let pp_pos loc =
    Format.sprintf "Line:%d Position:%d" loc.Lexing.pos_lnum
      (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

let pp ((s, e): t) = 
    Format.sprintf "(%s, %s)" (pp_pos s) (pp_pos e)

let dummy : t = (Lexing.dummy_pos, Lexing.dummy_pos)

(* other utilities go here *)
