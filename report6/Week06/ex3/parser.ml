type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
  | LET
  | IN
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | EQ
  | LT
  | IF
  | THEN
  | ELSE
  | LPAR
  | RPAR
  | FUN
  | ARROW
  | REC
  | AND
  | SEMISEMI

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 30 "parser.ml"
let yytransl_const = [|
  260 (* LET *);
  261 (* IN *);
  262 (* PLUS *);
  263 (* TIMES *);
  264 (* MINUS *);
  265 (* DIV *);
  266 (* EQ *);
  267 (* LT *);
  268 (* IF *);
  269 (* THEN *);
  270 (* ELSE *);
  271 (* LPAR *);
  272 (* RPAR *);
  273 (* FUN *);
  274 (* ARROW *);
  275 (* REC *);
  276 (* AND *);
  277 (* SEMISEMI *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\005\000\005\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\006\000\
\006\000\006\000\007\000\007\000\007\000\008\000\008\000\009\000\
\009\000\009\000\009\000\004\000\004\000\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\004\000\006\000\004\000\006\000\007\000\
\005\000\006\000\004\000\005\000\003\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\001\000\002\000\001\000\001\000\
\001\000\001\000\003\000\002\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\024\000\025\000\026\000\000\000\000\000\000\000\
\000\000\031\000\000\000\000\000\000\000\000\000\023\000\030\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\000\000\000\028\000\000\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\009\000\000\000\002\000\000\000\
\000\000\000\000\000\000\012\000\000\000\007\000\000\000\003\000\
\000\000\010\000\000\000\008\000\005\000"

let yydgoto = "\002\000\
\010\000\011\000\034\000\035\000\032\000\012\000\013\000\014\000\
\015\000"

let yysindex = "\004\000\
\014\255\000\000\000\000\000\000\000\000\005\255\172\255\172\255\
\011\255\000\000\034\255\194\255\038\255\193\255\000\000\000\000\
\011\255\033\255\019\255\051\255\062\255\001\255\000\000\193\255\
\193\255\193\255\193\255\193\255\193\255\000\000\011\255\028\255\
\172\255\011\255\071\255\011\255\053\255\172\255\000\000\172\255\
\082\255\038\255\038\255\045\255\045\255\193\255\193\255\091\255\
\172\255\000\000\036\255\000\000\172\255\097\255\172\255\102\255\
\101\255\000\000\172\255\172\255\000\000\172\255\000\000\039\255\
\112\255\172\255\172\255\000\000\098\255\000\000\172\255\000\000\
\114\255\000\000\011\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\007\255\117\255\066\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\032\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\134\255\151\255\165\255\177\255\083\255\100\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\041\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\250\255\061\000\221\255\239\255\043\000\056\000\
\244\255"

let yytablesize = 208
let yytable = "\018\000\
\054\000\030\000\022\000\016\000\001\000\020\000\021\000\016\000\
\044\000\045\000\031\000\015\000\037\000\016\000\003\000\004\000\
\005\000\006\000\040\000\015\000\015\000\016\000\015\000\017\000\
\048\000\007\000\015\000\015\000\008\000\031\000\009\000\051\000\
\049\000\030\000\030\000\016\000\057\000\036\000\058\000\077\000\
\062\000\029\000\033\000\071\000\028\000\006\000\029\000\061\000\
\050\000\029\000\024\000\064\000\025\000\065\000\023\000\016\000\
\063\000\068\000\069\000\072\000\070\000\006\000\055\000\038\000\
\073\000\074\000\042\000\043\000\031\000\076\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\039\000\021\000\021\000\
\053\000\021\000\041\000\046\000\047\000\021\000\021\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\052\000\019\000\
\019\000\056\000\019\000\059\000\060\000\049\000\019\000\019\000\
\020\000\020\000\020\000\020\000\020\000\020\000\020\000\066\000\
\020\000\020\000\067\000\020\000\062\000\075\000\071\000\020\000\
\020\000\018\000\018\000\000\000\018\000\000\000\018\000\018\000\
\000\000\018\000\018\000\000\000\018\000\000\000\000\000\000\000\
\018\000\018\000\016\000\016\000\000\000\016\000\000\000\016\000\
\016\000\000\000\016\000\016\000\000\000\016\000\000\000\000\000\
\000\000\016\000\016\000\017\000\017\000\000\000\017\000\000\000\
\017\000\017\000\000\000\017\000\017\000\000\000\017\000\000\000\
\000\000\013\000\017\000\017\000\003\000\004\000\005\000\019\000\
\000\000\013\000\013\000\000\000\013\000\014\000\000\000\007\000\
\013\000\013\000\008\000\000\000\009\000\014\000\014\000\000\000\
\014\000\003\000\004\000\005\000\014\000\014\000\000\000\024\000\
\000\000\025\000\000\000\026\000\027\000\000\000\000\000\008\000"

let yycheck = "\006\000\
\036\000\014\000\009\000\003\001\001\000\007\000\008\000\003\001\
\026\000\027\000\017\000\005\001\019\000\003\001\001\001\002\001\
\003\001\004\001\018\001\013\001\014\001\003\001\016\001\019\001\
\031\000\012\001\020\001\021\001\015\001\036\000\017\001\033\000\
\005\001\046\000\047\000\003\001\038\000\019\001\040\000\075\000\
\005\001\010\001\010\001\005\001\007\001\005\001\009\001\049\000\
\021\001\018\001\006\001\053\000\008\001\055\000\021\001\003\001\
\021\001\059\000\060\000\021\001\062\000\021\001\010\001\013\001\
\066\000\067\000\024\000\025\000\075\000\071\000\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\016\001\013\001\014\001\
\010\001\016\001\022\000\028\000\029\000\020\001\021\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\034\000\013\001\
\014\001\037\000\016\001\018\001\010\001\005\001\020\001\021\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\010\001\
\013\001\014\001\014\001\016\001\005\001\020\001\005\001\020\001\
\021\001\005\001\006\001\255\255\008\001\255\255\010\001\011\001\
\255\255\013\001\014\001\255\255\016\001\255\255\255\255\255\255\
\020\001\021\001\005\001\006\001\255\255\008\001\255\255\010\001\
\011\001\255\255\013\001\014\001\255\255\016\001\255\255\255\255\
\255\255\020\001\021\001\005\001\006\001\255\255\008\001\255\255\
\010\001\011\001\255\255\013\001\014\001\255\255\016\001\255\255\
\255\255\005\001\020\001\021\001\001\001\002\001\003\001\004\001\
\255\255\013\001\014\001\255\255\016\001\005\001\255\255\012\001\
\020\001\021\001\015\001\255\255\017\001\013\001\014\001\255\255\
\016\001\001\001\002\001\003\001\020\001\021\001\255\255\006\001\
\255\255\008\001\255\255\010\001\011\001\255\255\255\255\015\001"

let yynames_const = "\
  LET\000\
  IN\000\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIV\000\
  EQ\000\
  LT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  LPAR\000\
  RPAR\000\
  FUN\000\
  ARROW\000\
  REC\000\
  AND\000\
  SEMISEMI\000\
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
# 23 "parser.mly"
                                     ( CExp _1 )
# 205 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 24 "parser.mly"
                                     ( CDecl (_2, _4) )
# 213 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 25 "parser.mly"
                                     ( CFunDecl (_2 :: _3, _5) )
# 222 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'let_and_decls) in
    Obj.repr(
# 26 "parser.mly"
                                     ( CRecDecl (_3) )
# 229 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'let_and_decls) in
    Obj.repr(
# 30 "parser.mly"
                                      ( (_1,_2,_4) :: _6 )
# 239 "parser.ml"
               : 'let_and_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 31 "parser.mly"
                                      ( [(_1,_2,_4)] )
# 248 "parser.ml"
               : 'let_and_decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                                  ( ELet(_2,_4,_6) )
# 257 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                                  ( ELetFun(_2 :: _3,_5,_7) )
# 267 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'let_and_decls) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                                  ( ELetRec(_3,_5) )
# 275 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                                  ( EIf(_2,_4,_6) )
# 284 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                                  ( EFun(_2,_4) )
# 292 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                                  ( EFuns(_2 :: _3,_5) )
# 301 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 40 "parser.mly"
                                  ( EEq(_1,_3) )
# 309 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 41 "parser.mly"
                                  ( ELt(_1,_3) )
# 317 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 42 "parser.mly"
                                  ( _1 )
# 324 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 46 "parser.mly"
                                 ( EAdd(_1,_3) )
# 332 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 47 "parser.mly"
                                 ( ESub(_1,_3) )
# 340 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 48 "parser.mly"
                                 ( _1 )
# 347 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 52 "parser.mly"
                               ( EMul(_1,_3) )
# 355 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 53 "parser.mly"
                               ( EDiv(_1,_3) )
# 363 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 54 "parser.mly"
                               ( _1 )
# 370 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 58 "parser.mly"
                         ( EApp(_1, _2) )
# 378 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 59 "parser.mly"
                         ( _1 )
# 385 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 62 "parser.mly"
                   ( EConstInt(_1) )
# 392 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 63 "parser.mly"
                   ( EConstBool(_1) )
# 399 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                   ( EVar(_1) )
# 406 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 65 "parser.mly"
                   ( _2 )
# 413 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vars) in
    Obj.repr(
# 69 "parser.mly"
                ( _1 :: _2)
# 421 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 70 "parser.mly"
                ( [_1] )
# 428 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 73 "parser.mly"
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
