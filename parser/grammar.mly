%{
    open Syntax
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token TRUE 
%token FALSE 

%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token COMMA
%token COLON SEMICOLON
%token EQUALS

%token CLAIM DEF
%token THE
%token DATA HAS

%token MATCH WITH UNDERSCORE BAR ARROW
%token LET MUT FN IN 
%token COLONEQUALS

%token IF THEN ELSE 

%token PLUS STAR MINUS DIV

%token GT LT GTEQUALS LTEQUALS

%token EOF

%start <Ast.toplevel list> toplevel

%%

toplevel: 
    | EOF { [] }
