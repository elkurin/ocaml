type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | LET
  | IN
  | LAND
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | AND
  | OR
  | EQ
  | LT
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | SEMISEMI
  | ERROR

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 30 "parser.ml"
let yytransl_const = [|
  260 (* LET *);
  261 (* IN *);
  262 (* LAND *);
  263 (* PLUS *);
  264 (* TIMES *);
  265 (* MINUS *);
  266 (* DIV *);
  267 (* AND *);
  268 (* OR *);
  269 (* EQ *);
  270 (* LT *);
  271 (* IF *);
  272 (* THEN *);
  273 (* ELSE *);
  274 (* LPAR *);
  275 (* RPAR *);
  276 (* SEMISEMI *);
  277 (* ERROR *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\005\000\
\005\000\006\000\006\000\003\000\003\000\003\000\003\000\007\000\
\007\000\008\000\008\000\008\000\009\000\009\000\009\000\010\000\
\010\000\010\000\011\000\011\000\011\000\011\000\004\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\006\000\009\000\004\000\005\000\004\000\
\005\000\003\000\005\000\006\000\006\000\003\000\001\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\001\000\003\000\
\003\000\001\000\001\000\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\027\000\028\000\029\000\000\000\000\000\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\026\000\031\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\014\000\016\000\000\000\000\000\000\000\000\000\024\000\
\025\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\003\000\000\000\012\000\000\000\004\000\000\000\
\013\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\006\000\000\000\008\000\005\000\007\000\009\000\011\000"

let yydgoto = "\002\000\
\009\000\051\000\010\000\052\000\055\000\056\000\011\000\012\000\
\013\000\014\000\015\000"

let yysindex = "\001\000\
\004\255\000\000\000\000\000\000\000\000\007\255\012\255\012\255\
\000\000\001\255\021\255\018\255\002\255\049\255\000\000\000\000\
\025\255\007\255\033\255\039\255\000\000\012\255\022\255\022\255\
\022\255\022\255\022\255\022\255\022\255\012\255\052\255\012\255\
\000\000\000\000\000\000\002\255\002\255\049\255\049\255\000\000\
\000\000\112\255\012\255\050\255\007\255\012\255\007\255\000\000\
\067\255\012\255\000\000\062\255\000\000\069\255\000\000\079\255\
\000\000\012\255\012\255\012\255\253\254\014\255\072\255\007\255\
\000\000\007\255\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\159\255\031\255\108\255\057\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\125\255\142\255\074\255\091\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\084\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\035\000\252\255\250\255\040\000\043\000\078\000\000\000\
\020\000\026\000\105\000"

let yytablesize = 179
let yytable = "\017\000\
\064\000\001\000\019\000\020\000\003\000\004\000\005\000\006\000\
\026\000\016\000\027\000\031\000\003\000\004\000\005\000\018\000\
\065\000\034\000\007\000\066\000\021\000\008\000\003\000\004\000\
\005\000\042\000\007\000\044\000\023\000\008\000\024\000\025\000\
\022\000\067\000\017\000\017\000\017\000\030\000\049\000\008\000\
\054\000\053\000\017\000\036\000\037\000\057\000\017\000\017\000\
\032\000\017\000\017\000\038\000\039\000\061\000\062\000\063\000\
\028\000\033\000\029\000\054\000\023\000\023\000\023\000\023\000\
\043\000\023\000\050\000\023\000\023\000\023\000\023\000\046\000\
\023\000\023\000\058\000\023\000\023\000\021\000\021\000\021\000\
\021\000\059\000\021\000\060\000\021\000\021\000\021\000\021\000\
\010\000\021\000\021\000\068\000\021\000\021\000\022\000\022\000\
\022\000\022\000\069\000\022\000\035\000\022\000\022\000\022\000\
\022\000\070\000\022\000\022\000\071\000\022\000\022\000\020\000\
\020\000\020\000\000\000\045\000\046\000\047\000\020\000\020\000\
\020\000\020\000\000\000\020\000\020\000\000\000\020\000\020\000\
\018\000\018\000\018\000\048\000\040\000\041\000\000\000\018\000\
\018\000\018\000\018\000\000\000\018\000\018\000\000\000\018\000\
\018\000\019\000\019\000\019\000\000\000\000\000\000\000\000\000\
\019\000\019\000\019\000\019\000\000\000\019\000\019\000\000\000\
\019\000\019\000\015\000\015\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\015\000\
\000\000\015\000\015\000"

let yycheck = "\006\000\
\004\001\001\000\007\000\008\000\001\001\002\001\003\001\004\001\
\007\001\003\001\009\001\018\000\001\001\002\001\003\001\004\001\
\020\001\022\000\015\001\006\001\020\001\018\001\001\001\002\001\
\003\001\030\000\015\001\032\000\011\001\018\001\013\001\014\001\
\012\001\020\001\004\001\005\001\006\001\013\001\043\000\018\001\
\047\000\046\000\012\001\024\000\025\000\050\000\016\001\017\001\
\016\001\019\001\020\001\026\000\027\000\058\000\059\000\060\000\
\008\001\019\001\010\001\066\000\004\001\005\001\006\001\007\001\
\013\001\009\001\017\001\011\001\012\001\013\001\014\001\005\001\
\016\001\017\001\013\001\019\001\020\001\004\001\005\001\006\001\
\007\001\013\001\009\001\005\001\011\001\012\001\013\001\014\001\
\005\001\016\001\017\001\020\001\019\001\020\001\004\001\005\001\
\006\001\007\001\064\000\009\001\023\000\011\001\012\001\013\001\
\014\001\066\000\016\001\017\001\066\000\019\001\020\001\004\001\
\005\001\006\001\255\255\004\001\005\001\006\001\011\001\012\001\
\013\001\014\001\255\255\016\001\017\001\255\255\019\001\020\001\
\004\001\005\001\006\001\020\001\028\000\029\000\255\255\011\001\
\012\001\013\001\014\001\255\255\016\001\017\001\255\255\019\001\
\020\001\004\001\005\001\006\001\255\255\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\255\255\016\001\017\001\255\255\
\019\001\020\001\004\001\005\001\006\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\016\001\017\001\
\255\255\019\001\020\001"

let yynames_const = "\
  LET\000\
  IN\000\
  LAND\000\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIV\000\
  AND\000\
  OR\000\
  EQ\000\
  LT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LPAR\000\
  RPAR\000\
  SEMISEMI\000\
  ERROR\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 24 "parser.mly"
                  ( CExp _1 )
# 196 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                             ( CDecl (_2, _4) )
# 204 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Syntax.command) in
    Obj.repr(
# 26 "parser.mly"
                                 ( DDecl (_2, _4, _6) )
# 213 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'la_expr) in
    Obj.repr(
# 27 "parser.mly"
                                 ( DDecl (_2, _4, _6) )
# 222 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'lai_expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 28 "parser.mly"
                                                   ( DLai (_2, _4, _6, _8) )
# 232 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 32 "parser.mly"
                             ( CDecl (_1, _3) )
# 240 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Syntax.command) in
    Obj.repr(
# 33 "parser.mly"
                             ( DDecl (_1, _3, _5) )
# 249 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                             ( CDecl (_1, _3) )
# 257 "parser.ml"
               : 'la_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'la_expr) in
    Obj.repr(
# 38 "parser.mly"
                             ( NDecl (_1, _3, _5) )
# 266 "parser.ml"
               : 'la_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                              ( CDecl (_1, _3) )
# 274 "parser.ml"
               : 'lai_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'lai_expr) in
    Obj.repr(
# 43 "parser.mly"
                              ( NDecl (_1, _3, _5) )
# 283 "parser.ml"
               : 'lai_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 47 "parser.mly"
                                ( ELet(_2,_4,_6) )
# 292 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                                ( EIf(_2,_4,_6) )
# 301 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                                ( EOr(_1,_3) )
# 309 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 50 "parser.mly"
                                ( _1 )
# 316 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 54 "parser.mly"
                            ( EAnd(_1,_3) )
# 324 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comp_expr) in
    Obj.repr(
# 55 "parser.mly"
                            ( _1 )
# 331 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 59 "parser.mly"
                            ( EEq(_1,_3) )
# 339 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 60 "parser.mly"
                            ( ELt(_1,_3) )
# 347 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 61 "parser.mly"
                            ( _1 )
# 354 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 65 "parser.mly"
                                 ( EAdd(_1,_3) )
# 362 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 66 "parser.mly"
                                 ( ESub(_1,_3) )
# 370 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 67 "parser.mly"
                                 ( _1 )
# 377 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 71 "parser.mly"
                                  ( EMul(_1,_3) )
# 385 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 72 "parser.mly"
                                  ( EDiv(_1,_3) )
# 393 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 73 "parser.mly"
                                  ( _1 )
# 400 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 77 "parser.mly"
                   ( EConstInt(_1) )
# 407 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 78 "parser.mly"
                   ( EConstBool(_1) )
# 414 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 79 "parser.mly"
                   ( EVar(_1) )
# 421 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                   ( _2 )
# 428 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 84 "parser.mly"
       ( _1 )
# 435 "parser.ml"
               : 'var))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.command)
