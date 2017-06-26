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
  | DFUN
  | REC
  | LBRACKET
  | RBRACKET
  | CONS
  | COMMA
  | SEMISEMI
  | ERROR

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 35 "parser.ml"
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
  275 (* DFUN *);
  276 (* REC *);
  277 (* LBRACKET *);
  278 (* RBRACKET *);
  279 (* CONS *);
  280 (* COMMA *);
  281 (* SEMISEMI *);
  282 (* ERROR *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\006\000\006\000\
\005\000\005\000\005\000\007\000\007\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\009\000\009\000\004\000\004\000\
\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\007\000\006\000\007\000\008\000\006\000\
\004\000\005\000\004\000\003\000\003\000\001\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\001\000\002\000\001\000\
\001\000\001\000\005\000\002\000\001\000\003\000\002\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\025\000\026\000\029\000\000\000\000\000\000\000\
\000\000\000\000\000\000\034\000\000\000\000\000\014\000\000\000\
\000\000\024\000\033\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\028\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\030\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\015\000\000\000\000\000\
\000\000\000\000\031\000\000\000\000\000\000\000\000\000\000\000\
\000\000\009\000\000\000\011\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\027\000\010\000\000\000\005\000\
\000\000\003\000\000\000\000\000\008\000\000\000\004\000\006\000\
\000\000\007\000"

let yydgoto = "\002\000\
\012\000\013\000\039\000\040\000\014\000\015\000\016\000\017\000\
\018\000"

let yysindex = "\028\000\
\007\255\000\000\000\000\000\000\000\000\045\255\196\255\196\255\
\048\255\048\255\034\255\000\000\062\255\067\255\000\000\008\255\
\043\255\000\000\000\000\048\255\051\255\047\255\080\255\252\254\
\022\255\079\255\000\000\000\000\043\255\043\255\043\255\043\255\
\043\255\043\255\043\255\000\000\048\255\196\255\048\255\098\255\
\048\255\056\255\196\255\000\000\196\255\196\255\093\255\196\255\
\008\255\008\255\086\255\086\255\024\255\000\000\043\255\043\255\
\105\255\254\254\000\000\196\255\048\255\196\255\106\255\103\255\
\102\255\000\000\196\255\000\000\196\255\196\255\000\000\002\255\
\119\255\127\255\196\255\196\255\000\000\000\000\009\255\000\000\
\196\255\000\000\196\255\129\255\000\000\196\255\000\000\000\000\
\130\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\205\255\000\000\138\255\
\075\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\159\255\180\255\209\255\222\255\205\255\000\000\096\255\117\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\249\255\252\255\010\000\039\000\103\000\066\000\079\000\
\243\255"

let yytablesize = 247
let yytable = "\023\000\
\024\000\021\000\070\000\036\000\025\000\026\000\081\000\003\000\
\004\000\005\000\006\000\044\000\032\000\086\000\034\000\037\000\
\035\000\042\000\007\000\045\000\032\000\008\000\071\000\009\000\
\019\000\010\000\082\000\011\000\001\000\029\000\058\000\030\000\
\057\000\087\000\047\000\064\000\061\000\065\000\066\000\046\000\
\068\000\036\000\036\000\003\000\004\000\005\000\033\000\019\000\
\059\000\019\000\019\000\063\000\072\000\019\000\074\000\027\000\
\073\000\008\000\019\000\078\000\038\000\079\000\080\000\011\000\
\020\000\062\000\041\000\084\000\085\000\051\000\052\000\053\000\
\029\000\088\000\030\000\089\000\031\000\032\000\090\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\028\000\022\000\
\022\000\033\000\022\000\029\000\043\000\030\000\049\000\050\000\
\048\000\022\000\022\000\022\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\060\000\020\000\020\000\067\000\020\000\
\055\000\056\000\069\000\075\000\076\000\077\000\020\000\020\000\
\020\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\083\000\021\000\021\000\070\000\021\000\081\000\086\000\054\000\
\000\000\000\000\000\000\021\000\021\000\021\000\019\000\019\000\
\000\000\019\000\000\000\019\000\019\000\000\000\019\000\019\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\019\000\019\000\019\000\017\000\017\000\000\000\017\000\000\000\
\017\000\017\000\000\000\017\000\017\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\017\000\017\000\017\000\
\018\000\018\000\000\000\018\000\000\000\018\000\018\000\000\000\
\018\000\018\000\000\000\018\000\003\000\004\000\005\000\022\000\
\000\000\000\000\018\000\018\000\018\000\000\000\000\000\007\000\
\000\000\016\000\008\000\000\000\009\000\012\000\010\000\000\000\
\011\000\016\000\016\000\000\000\016\000\012\000\012\000\000\000\
\012\000\000\000\013\000\000\000\016\000\016\000\000\000\000\000\
\012\000\012\000\013\000\013\000\000\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\013\000"

let yycheck = "\007\000\
\008\000\006\000\005\001\017\000\009\000\010\000\005\001\001\001\
\002\001\003\001\004\001\016\001\010\001\005\001\007\001\020\000\
\009\001\022\000\012\001\024\001\018\001\015\001\025\001\017\001\
\003\001\019\001\025\001\021\001\001\000\006\001\038\000\008\001\
\037\000\025\001\025\000\043\000\041\000\045\000\046\000\018\001\
\048\000\055\000\056\000\001\001\002\001\003\001\023\001\003\001\
\039\000\003\001\003\001\042\000\060\000\003\001\062\000\022\001\
\061\000\015\001\003\001\067\000\010\001\069\000\070\000\021\001\
\020\001\010\001\020\001\075\000\076\000\031\000\032\000\033\000\
\006\001\081\000\008\001\083\000\010\001\011\001\086\000\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\025\001\013\001\
\014\001\023\001\016\001\006\001\013\001\008\001\029\000\030\000\
\018\001\023\001\024\001\025\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\010\001\013\001\014\001\018\001\016\001\
\034\000\035\000\010\001\010\001\014\001\016\001\023\001\024\001\
\025\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\010\001\013\001\014\001\005\001\016\001\005\001\005\001\033\000\
\255\255\255\255\255\255\023\001\024\001\025\001\005\001\006\001\
\255\255\008\001\255\255\010\001\011\001\255\255\013\001\014\001\
\255\255\016\001\255\255\255\255\255\255\255\255\255\255\255\255\
\023\001\024\001\025\001\005\001\006\001\255\255\008\001\255\255\
\010\001\011\001\255\255\013\001\014\001\255\255\016\001\255\255\
\255\255\255\255\255\255\255\255\255\255\023\001\024\001\025\001\
\005\001\006\001\255\255\008\001\255\255\010\001\011\001\255\255\
\013\001\014\001\255\255\016\001\001\001\002\001\003\001\004\001\
\255\255\255\255\023\001\024\001\025\001\255\255\255\255\012\001\
\255\255\005\001\015\001\255\255\017\001\005\001\019\001\255\255\
\021\001\013\001\014\001\255\255\016\001\013\001\014\001\255\255\
\016\001\255\255\005\001\255\255\024\001\025\001\255\255\255\255\
\024\001\025\001\013\001\014\001\255\255\016\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\024\001\025\001"

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
  DFUN\000\
  REC\000\
  LBRACKET\000\
  RBRACKET\000\
  CONS\000\
  COMMA\000\
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
# 26 "parser.mly"
                                     ( CExp _1 )
# 238 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                                     ( CDecl (_2, _4) )
# 246 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 28 "parser.mly"
                                     ( CFunDecl (_2 :: _3, _5) )
# 255 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 29 "parser.mly"
                                     ( CRecDecl (_3, _4, _6) )
# 264 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                                    ( ELet(_2,_4,_6) )
# 273 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 34 "parser.mly"
                                    ( ELetFun(_2 :: _3,_5,_7) )
# 283 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 35 "parser.mly"
                                    ( ELetRec(_3,_4,_6,_8) )
# 293 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                                    ( EIf(_2,_4,_6) )
# 302 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                                    ( EFun(_2,_4) )
# 310 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                                    ( EFuns(_2 :: _3,_5) )
# 319 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                                    ( EDFun(_2,_4) )
# 327 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 40 "parser.mly"
                                    ( EEq(_1,_3) )
# 335 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 41 "parser.mly"
                                    ( ELt(_1,_3) )
# 343 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list_expr) in
    Obj.repr(
# 42 "parser.mly"
                                    ( _1 )
# 350 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list_expr) in
    Obj.repr(
# 46 "parser.mly"
                              ( ECons(_1, _3) )
# 358 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 47 "parser.mly"
                              ( _1 )
# 365 "parser.ml"
               : 'list_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 51 "parser.mly"
                                 ( EAdd(_1,_3) )
# 373 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 52 "parser.mly"
                                 ( ESub(_1,_3) )
# 381 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 53 "parser.mly"
                                 ( _1 )
# 388 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 57 "parser.mly"
                               ( EMul(_1,_3) )
# 396 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 58 "parser.mly"
                               ( EDiv(_1,_3) )
# 404 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 59 "parser.mly"
                               ( _1 )
# 411 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 63 "parser.mly"
                         ( EApp(_1, _2) )
# 419 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 64 "parser.mly"
                         ( _1 )
# 426 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 67 "parser.mly"
                   ( EConstInt(_1) )
# 433 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 68 "parser.mly"
                   ( EConstBool(_1) )
# 440 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                              ( EPair(_2, _4) )
# 448 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                      ( ENil )
# 454 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                   ( EVar(_1) )
# 461 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                   ( _2 )
# 468 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vars) in
    Obj.repr(
# 76 "parser.mly"
                ( _1 :: _2)
# 476 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 77 "parser.mly"
                ( [_1] )
# 483 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 80 "parser.mly"
       ( _1 )
# 490 "parser.ml"
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
