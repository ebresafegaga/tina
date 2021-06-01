{
    open Lexing 
    open Grammar

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with 
                pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1 }
}


let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+
let frac = '.' digit*
let float = digit* frac?

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read_token = parse 
    | "true" { TRUE }   
    | "false" { FALSE }
    | "("  { LPAREN }
    | ")" { RPAREN }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "[" { LBRACK }
    | "]" { RBRACK }
    | "," { COMMA }
    | ":" { COLON }
    | ";" { SEMICOLON }
    | "=" { EQUALS }
    | "claim" { CLAIM }
    | "def" { DEF }
    | "the" { THE }
    | "data" { DATA } 
    | "ability" { ABILITY }
    | "has" { HAS }
    | "match" { MATCH }
    | "with" { WITH }
    | "end" { END }
    | "_" { UNDERSCORE }
    | "->" { ARROW }
    | "let" { LET }
    | "mut" { MUT }
    | "fn" { FN }
    | ":=" { COLONEQUALS }
    | "if" { IF }
    | "then" { THEN }
    | "else" { ELSE }
    | "+" { PLUS }
    | "*" { STAR }
    | "-" { MINUS }
    | "/" { DIV }
    | ">" { GT }
    | ">=" { GTEQUALS }
    | "<" { LT }
    | "<=" { LTEQUALS }
    | "Nat" { TY_NAT }
    | "Float" { TY_FLOAT }
    | "String" { TY_STRING }
    | "INT" { TY_INT }
    | "TODO" { TK_TODO }
    | '"' { read_string (Buffer.create 17) lexbuf }
    | whitespace { read_token lexbuf }
    | newline  { next_line lexbuf; read_token lexbuf }
    | id { ID (Lexing.lexeme lexbuf) }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | eof { EOF }

and read_string buf = parse
    | '"'       { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
    }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError ("String is not terminated")) }