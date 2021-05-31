
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

type t = Lexing.position

let dummy = 
    Lexing.{ pos_fname = "DUMMY"; 
             pos_lnum = 0; 
             pos_bol = 0; 
             pos_cnum = 0 }

(* other utilities go here *)