%{
    open Syntax
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

%token EOF

%start <Ast.toplevel list> toplevel

%%

toplevel: 
    | EOF { [] }
