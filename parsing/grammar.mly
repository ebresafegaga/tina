%{
    open Syntax
    open Ast
    open Naming

    let empty_variants : VarName.t list ref = ref []
    let non_empty_variants : VarName.t list ref = ref []
    let collect_empty_variants = function 
        | VariantDef (_loc, _name, body) -> 
            let mt, non_mt = 
                body 
                |> List.partition_map (function name, [] -> Either.Left name | name, _ -> Either.Right name)
            in 
            empty_variants := mt @ !empty_variants;
	    non_empty_variants := non_mt @ !non_empty_variants
        | _ -> ()
    let is_empty_variant x = List.mem x !empty_variants
    let is_non_empty_variant x = List.mem x !non_empty_variants

   let is_variable = function
      | Variable (_, var) -> Some var
      | _ -> None

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
%token EQUALS BAR 

%token CLAIM DEF
%token THE
%token DATA

%token CASE ARROW
%token LET MUT FN END
%token COLONEQUALS

%token IF THEN ELSE 

%token PLUS STAR MINUS DIV

%token GT LT GTEQUALS LTEQUALS

%token TY_NAT TY_STRING TY_FLOAT TY_INT

%token TK_TODO

%token DO HANDLE ABILITY RETURN

%token EOF

%left PLUS MINUS
%left MULT DIV

%start toplevel

%type <Ast.toplevel list> toplevel
%type <Type.t> ty
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
    | v = variant_decl { v }
    | a = ability { a }
    | e = expression { Expression e }

claim: 
    | CLAIM; id = ID; t = ty; 
    { Claim ($loc, VarName.of_string id, t) }

ability:
  | ABILITY; id = ID; LPAREN; tys = separated_list(COMMA, ty); RPAREN
    { AbilityDef ($loc, VarName.of_string id, tys) }

def: 
    | DEF; id = ID; EQUALS; body = expression 
      { Def ($loc, VarName.of_string id, body) }
    | DEF; id = ID; args = arg_list; EQUALS; body = expression;
      { Def ($loc, VarName.of_string id, Fn ($loc, args, body)) } 

record_claim: 
    | CLAIM; id = ID; t = ty; { (FieldName.of_string id, t) }

record_decl: 
    | DATA; id = ID; EQUALS; LBRACE; claims = separated_nonempty_list(COMMA, record_claim); RBRACE
      { RecordDef ($loc, DataName.of_string id, claims)  }


single_variant:
    | name = ID { (VarName.of_string name, []) } 
    | name = ID; LPAREN; tys = separated_nonempty_list(COMMA, ty); RPAREN 
      { (VarName.of_string name, tys) }

variant_decl: 
    | DATA; name = ID; EQUALS; body = separated_nonempty_list(BAR, single_variant);
      { let variant_def = VariantDef ($loc, DataName.of_string name, body) in 
        collect_empty_variants variant_def;
        variant_def }


record_expr_body: 
    | field = ID; COLON; e = expression; { (FieldName.of_string field, e) }

record_pattern_expr: 
    | field = ID; COLON; p = pattern; { (FieldName.of_string field, p) }

ty: 
    | TY_NAT { TyNat }
    | TY_STRING { TyString }
    | TY_FLOAT { TyFloat }
    | TY_INT { TyInt }
    | LPAREN;  t = ty; RPAREN { t }
    | LPAREN; body = separated_nonempty_list(COMMA, ty); RPAREN 
      { TyTuple (body) }
    | LPAREN a = separated_nonempty_list(COMMA, ty); ARROW; b = ty; RPAREN 
      { TyArrow (a, b) }
    

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
    | i = INT { PInteger (i) }
    | TRUE { PBool (true) }
    | FALSE { PBool (false) }
    | s = STRING { PString (s) }
    | LPAREN; p = pattern; RPAREN; { p }
    | LPAREN; body = separated_nonempty_list(COMMA, pattern); RPAREN
      { PTuple (body) }
    | id = ID 
        {   let name = VarName.of_string id in
            if is_empty_variant name then 
                PVariant (name, [])
            else 
                PVariable (name) 
        }
    | rec_name = ID; LBRACE; body = separated_nonempty_list(COMMA, record_pattern_expr); RBRACE; 
      { PRecord (DataName.of_string rec_name, body) }
    | variant_name = ID; LPAREN; body = separated_nonempty_list(COMMA, pattern); RPAREN; 
      { PVariant (VarName.of_string variant_name, body) }

expression: 
    | name = ID
    { if is_empty_variant (VarName.of_string name) then
	  Variant ($loc, DataName.of_string name, [])
      else
	  Variable ($loc, VarName.of_string name) }
    | LPAREN; RPAREN { LitUnit ($loc) }

    | value = INT { LitInteger ($loc, value) }
    | value = FLOAT { LitFloat ($loc, value) }
    | value = STRING { LitString ($loc, value) }
    | TRUE { LitBool ($loc, true) }
    | FALSE { LitBool ($loc, false) }
    | TK_TODO { LitTodo ($loc) }
    
    | IF; pred = expression; THEN pred_true = expression; ELSE pred_false = expression
     { If ($loc, pred, pred_true, pred_false) }
    | LET; pat = pattern; EQUALS; value = expression; SEMICOLON; body = maybe_empty_expr
      { Let ($loc, pat, value, body) } 
    | FN; args = arg_list; body = expression;
      { Fn ($loc, args, body) }
    | THE; t = ty; e = expression; 
      { Annotated ($loc, e, t) }
    | f = expression; args = expr_list;
    { match is_variable f with
      | Some var -> 
	 if is_non_empty_variant var then
	     Variant ($loc, DataName.of_string (VarName.to_string var), args)
	 else
	     Application ($loc, f, args)
      | None ->
	  Application ($loc, f, args) }
    | e1 = expression; SEMICOLON; e2 = expression 
      { Sequence ($loc, e1, e2) }
    | record = expression; DOT; field = ID;
      { RecordIndex ($loc, record, FieldName.of_string field) }
    | name = ID; LBRACE; body = separated_nonempty_list(COMMA, record_expr_body); RBRACE 
      { Record ($loc, DataName.of_string name, body) }
    | CASE; e = expression; LBRACE; body = separated_nonempty_list(COMMA, case_expr_body); RBRACE
      { Case ($loc, e, body) } 
    | e1 = expression; op = operator; e2 = expression 
      { let o = Variable ($loc, VarName.of_string op) in 
         Application ($loc, o, [e1; e2]) }
    | LPAREN; e = expression; RPAREN  { e }
  | LPAREN; body = separated_nonempty_list(COMMA, expression); RPAREN { Tuple ($loc, body) }
  | DO; name = ID; LPAREN; body = separated_list(COMMA, expression); RPAREN
    { Do ($loc, VarName.of_string name, body) }
  | HANDLE; e = expression; LBRACE;
           ret = return_clause; COMMA;
           body=separated_list(COMMA, handler_clause) RBRACE 
    { Handle ($loc, e, ret :: body) }

return_clause:
  | RETURN; id = ID; ARROW; e = expression; { Return (VarName.of_string id, e) } 

handler_clause:
  | name = ID; LPAREN; body = separated_list(COMMA, ID); RPAREN; kvar = ID; ARROW; e = expression
    {   let body = List.map VarName.of_string body in
	Operation (VarName.of_string name, body, VarName.of_string kvar, e) }
   
  
%inline operator: 
    | PLUS {  "+" }
    | MINUS { "-" }
    | STAR { "*" } 
    | DIV { "/" }
