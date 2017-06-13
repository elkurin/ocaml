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
  | AND
  | SEMISEMI
  | ERROR

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 32 "parser.ml"
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
  277 (* AND *);
  278 (* SEMISEMI *);
  279 (* ERROR *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\005\000\005\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\006\000\006\000\006\000\007\000\007\000\007\000\008\000\008\000\
\009\000\009\000\009\000\009\000\004\000\004\000\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\004\000\006\000\004\000\006\000\007\000\
\005\000\006\000\004\000\005\000\004\000\003\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\001\000\002\000\001\000\
\001\000\001\000\001\000\003\000\002\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\025\000\026\000\027\000\000\000\000\000\000\000\
\000\000\000\000\032\000\000\000\000\000\000\000\000\000\024\000\
\031\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\004\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\011\000\000\000\013\000\000\000\
\009\000\000\000\002\000\000\000\000\000\000\000\000\000\012\000\
\000\000\007\000\000\000\003\000\000\000\010\000\000\000\008\000\
\005\000"

let yydgoto = "\002\000\
\011\000\012\000\036\000\037\000\034\000\013\000\014\000\015\000\
\016\000"

let yysindex = "\034\000\
\007\255\000\000\000\000\000\000\000\000\010\255\074\255\074\255\
\050\255\050\255\000\000\036\255\098\255\008\255\047\255\000\000\
\000\000\050\255\015\255\013\255\051\255\065\255\002\255\054\255\
\000\000\047\255\047\255\047\255\047\255\047\255\047\255\000\000\
\050\255\016\255\074\255\050\255\075\255\050\255\056\255\074\255\
\000\000\074\255\072\255\074\255\008\255\008\255\048\255\048\255\
\047\255\047\255\082\255\074\255\000\000\018\255\000\000\074\255\
\096\255\074\255\097\255\105\255\000\000\074\255\000\000\074\255\
\000\000\074\255\000\000\023\255\117\255\074\255\074\255\000\000\
\106\255\000\000\074\255\000\000\121\255\000\000\050\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\066\255\143\255\089\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\042\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\161\255\179\255\183\255\193\255\
\107\255\125\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\255\255\250\255\008\000\219\255\039\000\057\000\094\000\
\243\255"

let yytablesize = 215
let yytable = "\019\000\
\057\000\032\000\023\000\024\000\017\000\021\000\022\000\003\000\
\004\000\005\000\006\000\033\000\017\000\039\000\030\000\017\000\
\031\000\017\000\007\000\042\000\052\000\008\000\066\000\009\000\
\035\000\010\000\051\000\075\000\006\000\018\000\043\000\033\000\
\038\000\054\000\001\000\032\000\032\000\053\000\060\000\067\000\
\061\000\081\000\063\000\055\000\076\000\006\000\059\000\003\000\
\004\000\005\000\065\000\030\000\017\000\026\000\068\000\027\000\
\069\000\025\000\017\000\030\000\072\000\008\000\073\000\040\000\
\074\000\058\000\047\000\048\000\077\000\078\000\016\000\044\000\
\033\000\080\000\003\000\004\000\005\000\020\000\016\000\016\000\
\041\000\016\000\045\000\046\000\056\000\007\000\016\000\016\000\
\008\000\062\000\009\000\064\000\010\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\052\000\022\000\022\000\026\000\
\022\000\027\000\070\000\028\000\029\000\022\000\022\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\071\000\020\000\
\020\000\066\000\020\000\049\000\050\000\075\000\079\000\020\000\
\020\000\021\000\021\000\021\000\021\000\021\000\021\000\021\000\
\000\000\021\000\021\000\000\000\021\000\000\000\000\000\000\000\
\000\000\021\000\021\000\019\000\019\000\000\000\019\000\000\000\
\019\000\019\000\000\000\019\000\019\000\000\000\019\000\000\000\
\000\000\000\000\000\000\019\000\019\000\017\000\017\000\000\000\
\017\000\000\000\017\000\017\000\000\000\017\000\017\000\000\000\
\017\000\000\000\000\000\000\000\000\000\017\000\017\000\018\000\
\018\000\000\000\018\000\014\000\018\000\018\000\000\000\018\000\
\018\000\000\000\018\000\014\000\014\000\015\000\014\000\018\000\
\018\000\000\000\000\000\014\000\014\000\015\000\015\000\000\000\
\015\000\000\000\000\000\000\000\000\000\015\000\015\000"

let yycheck = "\006\000\
\038\000\015\000\009\000\010\000\003\001\007\000\008\000\001\001\
\002\001\003\001\004\001\018\000\003\001\020\000\007\001\003\001\
\009\001\003\001\012\001\018\001\005\001\015\001\005\001\017\001\
\010\001\019\001\033\000\005\001\005\001\020\001\023\000\038\000\
\020\001\035\000\001\000\049\000\050\000\022\001\040\000\022\001\
\042\000\079\000\044\000\036\000\022\001\022\001\039\000\001\001\
\002\001\003\001\052\000\010\001\003\001\006\001\056\000\008\001\
\058\000\022\001\003\001\018\001\062\000\015\001\064\000\013\001\
\066\000\010\001\028\000\029\000\070\000\071\000\005\001\018\001\
\079\000\075\000\001\001\002\001\003\001\004\001\013\001\014\001\
\016\001\016\001\026\000\027\000\010\001\012\001\021\001\022\001\
\015\001\018\001\017\001\010\001\019\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\005\001\013\001\014\001\006\001\
\016\001\008\001\010\001\010\001\011\001\021\001\022\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\014\001\013\001\
\014\001\005\001\016\001\030\000\031\000\005\001\021\001\021\001\
\022\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\013\001\014\001\255\255\016\001\255\255\255\255\255\255\
\255\255\021\001\022\001\005\001\006\001\255\255\008\001\255\255\
\010\001\011\001\255\255\013\001\014\001\255\255\016\001\255\255\
\255\255\255\255\255\255\021\001\022\001\005\001\006\001\255\255\
\008\001\255\255\010\001\011\001\255\255\013\001\014\001\255\255\
\016\001\255\255\255\255\255\255\255\255\021\001\022\001\005\001\
\006\001\255\255\008\001\005\001\010\001\011\001\255\255\013\001\
\014\001\255\255\016\001\013\001\014\001\005\001\016\001\021\001\
\022\001\255\255\255\255\021\001\022\001\013\001\014\001\255\255\
\016\001\255\255\255\255\255\255\255\255\021\001\022\001"

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
  AND\000\
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
# 25 "parser.mly"
                                     ( CExp _1 )
# 216 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 26 "parser.mly"
                                     ( CDecl (_2, _4) )
# 224 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                                     ( CFunDecl (_2 :: _3, _5) )
# 233 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'let_and_decls) in
    Obj.repr(
# 28 "parser.mly"
                                     ( CRecDecl (_3) )
# 240 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'let_and_decls) in
    Obj.repr(
# 32 "parser.mly"
                                      ( (_1,_2,_4) :: _6 )
# 250 "parser.ml"
               : 'let_and_decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                                      ( [(_1,_2,_4)] )
# 259 "parser.ml"
               : 'let_and_decls))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 36 "parser.mly"
                                  ( ELet(_2,_4,_6) )
# 268 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 37 "parser.mly"
                                  ( ELetFun(_2 :: _3,_5,_7) )
# 278 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'let_and_decls) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                                  ( ELetRec(_3,_5) )
# 286 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 39 "parser.mly"
                                  ( EIf(_2,_4,_6) )
# 295 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 40 "parser.mly"
                                  ( EFun(_2,_4) )
# 303 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'vars) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 41 "parser.mly"
                                  ( EFuns(_2 :: _3,_5) )
# 312 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 42 "parser.mly"
                                  ( EDFun(_2,_4) )
# 320 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 43 "parser.mly"
                                  ( EEq(_1,_3) )
# 328 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 44 "parser.mly"
                                  ( ELt(_1,_3) )
# 336 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 45 "parser.mly"
                                  ( _1 )
# 343 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 49 "parser.mly"
                                 ( EAdd(_1,_3) )
# 351 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 50 "parser.mly"
                                 ( ESub(_1,_3) )
# 359 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 51 "parser.mly"
                                 ( _1 )
# 366 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 55 "parser.mly"
                               ( EMul(_1,_3) )
# 374 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 56 "parser.mly"
                               ( EDiv(_1,_3) )
# 382 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_expr) in
    Obj.repr(
# 57 "parser.mly"
                               ( _1 )
# 389 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 61 "parser.mly"
                         ( EApp(_1, _2) )
# 397 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 62 "parser.mly"
                         ( _1 )
# 404 "parser.ml"
               : 'app_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "parser.mly"
                   ( EConstInt(_1) )
# 411 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 66 "parser.mly"
                   ( EConstBool(_1) )
# 418 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 67 "parser.mly"
                   ( EVar(_1) )
# 425 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 68 "parser.mly"
                   ( _2 )
# 432 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'var) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vars) in
    Obj.repr(
# 72 "parser.mly"
                ( _1 :: _2)
# 440 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 73 "parser.mly"
                ( [_1] )
# 447 "parser.ml"
               : 'vars))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 76 "parser.mly"
       ( _1 )
# 454 "parser.ml"
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
