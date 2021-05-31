%{
    open Syntax
    open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token TRUE 
%token FALSE 

%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token COMMA
%token COLON SEMICOLON
%token EQUALS

%token CLAIM DEF
%token THE
%token DATA ABILITY HAS

%token MATCH WITH UNDERSCORE BAR ARROW
%token LET MUT FN
%token COLONEQUALS

%token IF THEN ELSE 

%token PLUS STAR MINUS DIV

%token GT LT GTEQUALS LTEQUALS

%token TY_NAT TY_STRING TY_FLOAT TY_INT

%token TK_TODO

%token EOF

%start toplevel

%type <Ast.toplevel> toplevel
%type <Ast.ty> ty

%%

toplevel: 
    | EOF { Toplevel ([], [], []) }

claim: 
    | CLAIM; id = ID; t = ty; { [] }

def: 
    | DEF; id = ID; args = arg_list; EQUALS; body = expression; { [] } 

ty: 
    | TY_NAT { TyNat }
    | TY_STRING { TyString }
    | TY_FLOAT { TyFloat }
    | TY_INT { TyInt }
    | LPAREN a = separated_list(COMMA, ty); ARROW; b = ty; RPAREN { TyArrow (a, b) }

arg_list: 
    | LPAREN; params=separated_list(COMMA,param); RPAREN { param s}

param: 
    | id = ID; { id }

expression:
    | TK_TODO { TODO }