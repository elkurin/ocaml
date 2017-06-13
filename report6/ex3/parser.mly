%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL 
%token <string> ID
%token LET IN LAND		  
%token PLUS TIMES MINUS DIV
%token AND OR
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR 
%token FUN ARROW
%token SEMISEMI
%token ERROR

%start toplevel 
%type <Syntax.command> toplevel
%type <Syntax.command> let_expr
%% 

toplevel:
  | expr SEMISEMI { CExp $1 }
  | LET var EQ expr SEMISEMI { CDecl ($2, $4) }
  | LET var EQ expr LET let_expr { DDecl ($2, $4, $6) }
  | LET var EQ expr LAND la_expr { DDecl ($2, $4, $6) }
  | LET var EQ expr LAND lai_expr IN expr SEMISEMI { DLai ($2, $4, $6, $8) }
;

let_expr:
  | var EQ expr SEMISEMI     { CDecl ($1, $3) }
  | var EQ expr LET let_expr { DDecl ($1, $3, $5) }
;

la_expr:
  | var EQ expr SEMISEMI     { CDecl ($1, $3) }
  | var EQ expr LAND la_expr { DDecl ($1, $3, $5) }
;

lai_expr:
  | var EQ expr               { CDecl ($1, $3) }
  | var EQ expr LAND lai_expr { NDecl ($1, $3, $5) }
;

expr:
  | LET var EQ expr IN expr     { ELet($2,$4,$6) }
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | FUN var ARROW expr          { EFun($2, $4) }
  | and_expr OR expr            { EOr($1,$3) }
  | and_expr                    { $1 } 
;

and_expr:
  | comp_expr AND and_expr  { EAnd($1,$3) }
  | comp_expr               { $1 } 
;

comp_expr:
  | comp_expr EQ arith_expr { EEq($1,$3) }
  | comp_expr LT arith_expr { ELt($1,$3) }
  | arith_expr              { $1 }
;

arith_expr:
  | arith_expr PLUS factor_expr  { EAdd($1,$3) }
  | arith_expr MINUS factor_expr { ESub($1,$3) }
  | factor_expr                  { $1 }
;

factor_expr:
  | factor_expr TIMES atomic_expr { EMul($1,$3) }
  | factor_expr DIV atomic_expr   { EDiv($1,$3) }
  | atomic_expr                   { $1 }
;

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr RPAR { $2 }
;

var:
  | ID { $1 }
;
