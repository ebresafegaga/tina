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

%token MATCH WITH BAR ARROW END
%token LET FN IN 

%token IF THEN ELSE 

%token PLUS STAR MINUS DIV

%token SPACE 

%token EOF


%start <UntypedSyntax.toplevel list> toplevel

%%

toplevel: 
    | EOF { [] }
