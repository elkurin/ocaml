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
  | FUN
  | ARROW
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
  276 (* FUN *);
  277 (* ARROW *);
  278 (* SEMISEMI *);
  279 (* ERROR *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\005\000\
\005\000\006\000\006\000\003\000\003\000\003\000\003\000\003\000\
\007\000\007\000\008\000\008\000\008\000\009\000\009\000\009\000\
\010\000\010\000\010\000\011\000\011\000\011\000\011\000\004\000\
\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\006\000\009\000\004\000\005\000\004\000\
\005\000\003\000\005\000\006\000\006\000\004\000\003\000\001\000\
\003\000\001\000\003\000\003\000\001\000\003\000\003\000\001\000\
\003\000\003\000\001\000\001\000\001\000\001\000\003\000\001\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\028\000\029\000\030\000\000\000\000\000\000\000\
\000\000\033\000\000\000\000\000\000\000\000\000\000\000\027\000\
\032\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\031\000\000\000\015\000\017\000\000\000\000\000\
\000\000\000\000\025\000\026\000\000\000\000\000\000\000\014\000\
\000\000\000\000\000\000\002\000\000\000\000\000\003\000\000\000\
\012\000\000\000\004\000\000\000\013\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\006\000\000\000\008\000\005\000\
\007\000\009\000\011\000"

let yydgoto = "\002\000\
\010\000\055\000\011\000\056\000\059\000\060\000\012\000\013\000\
\014\000\015\000\016\000"

let yysindex = "\013\000\
\005\255\000\000\000\000\000\000\000\000\019\255\034\255\034\255\
\019\255\000\000\008\255\022\255\013\255\032\255\036\255\000\000\
\000\000\027\255\019\255\026\255\029\255\048\255\000\000\034\255\
\014\255\014\255\014\255\014\255\014\255\014\255\014\255\034\255\
\037\255\034\255\000\000\034\255\000\000\000\000\032\255\032\255\
\036\255\036\255\000\000\000\000\006\255\034\255\054\255\000\000\
\019\255\034\255\019\255\000\000\071\255\034\255\000\000\040\255\
\000\000\066\255\000\000\076\255\000\000\034\255\034\255\034\255\
\253\254\252\254\060\255\019\255\000\000\019\255\000\000\000\000\
\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\179\255\175\255\118\255\061\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\137\255\156\255\
\080\255\099\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\083\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\022\000\253\255\250\255\025\000\028\000\075\000\000\000\
\029\000\034\000\027\000"

let yytablesize = 201
let yytable = "\018\000\
\068\000\070\000\022\000\020\000\021\000\003\000\004\000\005\000\
\006\000\049\000\050\000\051\000\033\000\001\000\003\000\004\000\
\005\000\071\000\069\000\007\000\037\000\017\000\008\000\025\000\
\009\000\026\000\027\000\052\000\045\000\023\000\047\000\008\000\
\048\000\024\000\003\000\004\000\005\000\019\000\028\000\032\000\
\029\000\034\000\053\000\030\000\058\000\031\000\057\000\035\000\
\007\000\046\000\061\000\008\000\062\000\009\000\039\000\040\000\
\043\000\044\000\065\000\066\000\067\000\041\000\042\000\058\000\
\024\000\024\000\024\000\024\000\036\000\024\000\054\000\024\000\
\024\000\024\000\024\000\050\000\024\000\024\000\063\000\024\000\
\064\000\072\000\024\000\022\000\022\000\022\000\022\000\010\000\
\022\000\073\000\022\000\022\000\022\000\022\000\074\000\022\000\
\022\000\075\000\022\000\038\000\000\000\022\000\023\000\023\000\
\023\000\023\000\000\000\023\000\000\000\023\000\023\000\023\000\
\023\000\000\000\023\000\023\000\000\000\023\000\000\000\000\000\
\023\000\021\000\021\000\021\000\000\000\000\000\000\000\000\000\
\021\000\021\000\021\000\021\000\000\000\021\000\021\000\000\000\
\021\000\000\000\000\000\021\000\019\000\019\000\019\000\000\000\
\000\000\000\000\000\000\019\000\019\000\019\000\019\000\000\000\
\019\000\019\000\000\000\019\000\000\000\000\000\019\000\020\000\
\020\000\020\000\000\000\000\000\000\000\000\000\020\000\020\000\
\020\000\020\000\000\000\020\000\020\000\000\000\020\000\000\000\
\000\000\020\000\018\000\018\000\018\000\000\000\016\000\016\000\
\016\000\000\000\018\000\000\000\000\000\000\000\018\000\018\000\
\000\000\018\000\016\000\016\000\018\000\016\000\000\000\000\000\
\016\000"

let yycheck = "\006\000\
\004\001\006\001\009\000\007\000\008\000\001\001\002\001\003\001\
\004\001\004\001\005\001\006\001\019\000\001\000\001\001\002\001\
\003\001\022\001\022\001\015\001\024\000\003\001\018\001\011\001\
\020\001\013\001\014\001\022\001\032\000\022\001\034\000\018\001\
\036\000\012\001\001\001\002\001\003\001\004\001\007\001\013\001\
\009\001\016\001\046\000\008\001\051\000\010\001\050\000\019\001\
\015\001\013\001\054\000\018\001\013\001\020\001\026\000\027\000\
\030\000\031\000\062\000\063\000\064\000\028\000\029\000\070\000\
\004\001\005\001\006\001\007\001\021\001\009\001\017\001\011\001\
\012\001\013\001\014\001\005\001\016\001\017\001\013\001\019\001\
\005\001\022\001\022\001\004\001\005\001\006\001\007\001\005\001\
\009\001\068\000\011\001\012\001\013\001\014\001\070\000\016\001\
\017\001\070\000\019\001\025\000\255\255\022\001\004\001\005\001\
\006\001\007\001\255\255\009\001\255\255\011\001\012\001\013\001\
\014\001\255\255\016\001\017\001\255\255\019\001\255\255\255\255\
\022\001\004\001\005\001\006\001\255\255\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\255\255\016\001\017\001\255\255\
\019\001\255\255\255\255\022\001\004\001\005\001\006\001\255\255\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\255\255\
\016\001\017\001\255\255\019\001\255\255\255\255\022\001\004\001\
\005\001\006\001\255\255\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\255\255\016\001\017\001\255\255\019\001\255\255\
\255\255\022\001\004\001\005\001\006\001\255\255\004\001\005\001\
\006\001\255\255\012\001\255\255\255\255\255\255\016\001\017\001\
\255\255\019\001\016\001\017\001\022\001\019\001\255\255\255\255\
\022\001"

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
  FUN\000\
  ARROW\000\
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
# 213 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 26 "parser.mly"
                             ( CDecl (_2, _4) )
# 221 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Syntax.command) in
    Obj.repr(
# 27 "parser.mly"
                                 ( DDecl (_2, _4, _6) )
# 230 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'la_expr) in
    Obj.repr(
# 28 "parser.mly"
                                 ( DDecl (_2, _4, _6) )
# 239 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'lai_expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 29 "parser.mly"
                                                   ( DLai (_2, _4, _6, _8) )
# 249 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 33 "parser.mly"
                             ( CDecl (_1, _3) )
# 257 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Syntax.command) in
    Obj.repr(
# 34 "parser.mly"
                             ( DDecl (_1, _3, _5) )
# 266 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 38 "parser.mly"
                             ( CDecl (_1, _3) )
# 274 "parser.ml"
               : 'la_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'la_expr) in
    Obj.repr(
# 39 "parser.mly"
                             ( DDecl (_1, _3, _5) )
# 283 "parser.ml"
               : 'la_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 43 "parser.mly"
                              ( CDecl (_1, _3) )
# 291 "parser.ml"
               : 'lai_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'lai_expr) in
    Obj.repr(
# 44 "parser.mly"
                              ( NDecl (_1, _3, _5) )
# 300 "parser.ml"
               : 'lai_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 48 "parser.mly"
                                ( ELet(_2,_4,_6) )
# 309 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 49 "parser.mly"
                                ( EIf(_2,_4,_6) )
# 318 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 50 "parser.mly"
                                ( EFun(_2, _4) )
# 326 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 51 "parser.mly"
                                ( EOr(_1,_3) )
# 334 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 52 "parser.mly"
                                ( _1 )
# 341 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 56 "parser.mly"
                            ( EAnd(_1,_3) )
# 349 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comp_expr) in
    Obj.repr(
# 57 "parser.mly"
                            ( _1 )
# 356 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 61 "parser.mly"
                            ( EEq(_1,_3) )
# 364 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 62 "parser.mly"
                            ( ELt(_1,_3) )
# 372 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 63 "parser.mly"
                            ( _1 )
# 379 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 67 "parser.mly"
                                 ( EAdd(_1,_3) )
# 387 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 68 "parser.mly"
                                 ( ESub(_1,_3) )
# 395 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 69 "parser.mly"
                                 ( _1 )
# 402 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 73 "parser.mly"
                                  ( EMul(_1,_3) )
# 410 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 74 "parser.mly"
                                  ( EDiv(_1,_3) )
# 418 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 75 "parser.mly"
                                  ( _1 )
# 425 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 79 "parser.mly"
                   ( EConstInt(_1) )
# 432 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 80 "parser.mly"
                   ( EConstBool(_1) )
# 439 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 81 "parser.mly"
                   ( EVar(_1) )
# 446 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                   ( _2 )
# 453 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 86 "parser.mly"
       ( _1 )
# 460 "parser.ml"
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
