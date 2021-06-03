%{
    open Syntax
    open Ast
    open Naming 
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token TRUE 
%token FALSE 

%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token COMMA
%token COLON SEMICOLON DOT
%token EQUALS

%token CLAIM DEF
%token THE
%token DATA ABILITY HAS

%token CASE UNDERSCORE ARROW
%token LET MUT FN END
%token COLONEQUALS

%token IF THEN ELSE 

%token PLUS STAR MINUS DIV

%token GT LT GTEQUALS LTEQUALS

%token TY_NAT TY_STRING TY_FLOAT TY_INT

%token TK_TODO

%token EOF

%start toplevel

%type <Ast.toplevel list> toplevel
%type <Ast.ty> ty
%type <toplevel> claim 
%type <toplevel> def
%type <toplevel> record_decl
%type <expression> expression
%type <expression> maybe_empty_expr

%%

toplevel: 
    | tops = list(top); EOF { tops }

top: 
    | c = claim { c }
    | d = def { d }
    | r = record_decl { r }
    | e = expression { Expression e }

claim: 
    | CLAIM; id = ID; t = ty; 
    { Claim ($loc, VarName.of_string id, t) }

def: 
    | DEF; id = ID; EQUALS; body = expression 
      { Def ($loc, VarName.of_string id, body) }
    | DEF; id = ID; args = arg_list; EQUALS; body = expression; 
      { Def ($loc, VarName.of_string id, Fn ($loc, args, body)) } 

record_claim: 
    | CLAIM; id = ID; t = ty; { (FieldName.of_string id, t) }

record_decl: 
    | DATA; id = ID; EQUALS; LBRACE; claims = separated_nonempty_list(COMMA, record_claim); RBRACE
      { RecordDef ($loc, DataName.of_string id, claims) }

record_expr_body: 
    | field = ID; COLON; e = expression; { (FieldName.of_string field, e) }

record_pattern_expr: 
    | field = ID; COLON; p = pattern; { (FieldName.of_string field, p) }

ty: 
    | TY_NAT { TyNat }
    | TY_STRING { TyString }
    | TY_FLOAT { TyFloat }
    | TY_INT { TyInt }
    | LPAREN a = separated_list(COMMA, ty); ARROW; b = ty; RPAREN { TyArrow (a, b) }

arg_list: 
    | LPAREN; params=separated_list(COMMA,param); RPAREN 
     { List.map VarName.of_string params }

(* we can add optional type annotations for 
   fn or normal defs from here *)
param: 
    | id = ID; { id }

expr_list: 
    | LPAREN; params=separated_list(COMMA,expression); RPAREN { params }

maybe_empty_expr: 
    | (* empty *) { LitUnit ($loc) }
    | e = expression { e }

case_expr_body: 
    | pat = pattern; ARROW; e = expression 
      { (pat, e) }

pattern:
    | var = ID { PVariable (VarName.of_string var) }
    | rec_name = ID; LBRACE; body = separated_nonempty_list(COMMA, record_pattern_expr); RBRACE; 
      { PRecord (DataName.of_string rec_name, body) }

expression: 
    | name = ID { Variable ($loc, VarName.of_string name) }
    | LPAREN; RPAREN { LitUnit ($loc) }

    | value = INT { LitInteger ($loc, value) }
    | value = FLOAT { LitFloat ($loc, value) }
    | value = STRING { LitString ($loc, value) }
    | TRUE { LitBool ($loc, true) }
    | FALSE { LitBool ($loc, false) }
    | TK_TODO { LitTodo ($loc) }
    
    | IF; pred = expression; THEN pred_true = expression; ELSE pred_false = expression
     { If ($loc, pred, pred_true, pred_false) }
    | LET; id = ID; EQUALS; value = expression; SEMICOLON; body = maybe_empty_expr
      { Let ($loc, VarName.of_string id, value, body) } 
    | FN; args = arg_list; body = expression;
      { Fn ($loc, args, body) }
    | THE; t = ty; e = expression; 
      { Annotated ($loc, e, t) }
    | operand = expression; operator = expr_list;
     { Application ($loc, operand, operator) }
    | e1 = expression; SEMICOLON; e2 = expression 
      { Sequence ($loc, e1, e2) }
    | record = expression; DOT; field = ID;
      { RecordIndex ($loc, record, FieldName.of_string field) }
    | name = ID; LBRACE; body = separated_nonempty_list(COMMA, record_expr_body); RBRACE 
      { Record ($loc, DataName.of_string name, body) }
    | CASE; e = expression; LBRACE; body = separated_nonempty_list(COMMA, case_expr_body); RBRACE
      { Case ($loc, e, body) } 
    | LPAREN; e = expression; RPAREN  { e }