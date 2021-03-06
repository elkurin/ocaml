%{
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
%}

%token <int>    INT
%token <bool>   BOOL 
%token <string> ID 
%token PLUS 
%token EQ LT
%token IF THEN ELSE
%token LPAR RPAR 
%token SEMISEMI

%start toplevel 
%type <Syntax.command> toplevel
%% 

toplevel:
  | expr SEMISEMI { CExp $1 }
;

expr:
  | IF expr THEN expr ELSE expr { EIf($2,$4,$6) }
  | factor_expr EQ factor_expr    { EEq($1,$3) }
  | factor_expr LT factor_expr    { ELt($1,$3) }
  | factor_expr PLUS factor_expr { EAdd($1,$3) }
  | factor_expr                  { $1 } 
;

factor_expr: 
  | atomic_expr                 { $1 }
;

atomic_expr:
  | INT            { EConstInt($1) }
  | BOOL           { EConstBool($1) }
  | ID             { EVar($1) }
  | LPAR expr RPAR { $2 }
;
 

