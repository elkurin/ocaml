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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 28 "parser.ml"
let yytransl_const = [|
  260 (* LET *);
  261 (* IN *);
  262 (* PLUS *);
  263 (* TIMES *);
  264 (* MINUS *);
  265 (* DIV *);
  266 (* AND *);
  267 (* OR *);
  268 (* EQ *);
  269 (* LT *);
  270 (* IF *);
  271 (* THEN *);
  272 (* ELSE *);
  273 (* LPAR *);
  274 (* RPAR *);
  275 (* SEMISEMI *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\004\000\004\000\
\005\000\005\000\005\000\006\000\006\000\006\000\007\000\007\000\
\007\000\008\000\008\000\008\000\008\000\003\000\000\000"

let yylen = "\002\000\
\002\000\005\000\006\000\006\000\003\000\001\000\003\000\001\000\
\003\000\003\000\001\000\003\000\003\000\001\000\003\000\003\000\
\001\000\001\000\001\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\018\000\019\000\020\000\000\000\000\000\000\000\
\023\000\000\000\000\000\000\000\000\000\000\000\017\000\022\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\021\000\005\000\007\000\000\000\000\000\000\000\000\000\015\000\
\016\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\
\003\000\004\000"

let yydgoto = "\002\000\
\009\000\010\000\017\000\011\000\012\000\013\000\014\000\015\000"

let yysindex = "\001\000\
\002\255\000\000\000\000\000\000\000\000\004\255\007\255\007\255\
\000\000\254\254\032\255\110\255\021\255\028\255\000\000\000\000\
\010\255\004\255\033\255\038\255\000\000\007\255\011\255\011\255\
\011\255\011\255\011\255\011\255\011\255\007\255\039\255\007\255\
\000\000\000\000\000\000\021\255\021\255\028\255\028\255\000\000\
\000\000\013\255\007\255\042\255\007\255\000\000\058\255\007\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\109\255\015\255\076\255\034\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\088\255\100\255\049\255\064\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\249\255\048\000\050\000\000\000\060\000\070\000\080\000"

let yytablesize = 128
let yytable = "\019\000\
\020\000\001\000\003\000\004\000\005\000\006\000\016\000\003\000\
\004\000\005\000\018\000\003\000\004\000\005\000\034\000\007\000\
\021\000\045\000\008\000\008\000\007\000\030\000\042\000\008\000\
\044\000\008\000\026\000\008\000\027\000\008\000\008\000\046\000\
\008\000\008\000\028\000\047\000\029\000\049\000\014\000\014\000\
\050\000\014\000\022\000\014\000\014\000\014\000\014\000\032\000\
\014\000\014\000\043\000\014\000\014\000\012\000\012\000\033\000\
\012\000\048\000\012\000\012\000\012\000\012\000\045\000\012\000\
\012\000\031\000\012\000\012\000\013\000\013\000\000\000\013\000\
\035\000\013\000\013\000\013\000\013\000\000\000\013\000\013\000\
\011\000\013\000\013\000\036\000\037\000\011\000\011\000\011\000\
\011\000\000\000\011\000\011\000\009\000\011\000\011\000\038\000\
\039\000\009\000\009\000\009\000\009\000\000\000\009\000\009\000\
\010\000\009\000\009\000\040\000\041\000\010\000\010\000\010\000\
\010\000\006\000\010\000\010\000\000\000\010\000\010\000\023\000\
\000\000\024\000\025\000\006\000\006\000\000\000\006\000\006\000"

let yycheck = "\007\000\
\008\000\001\000\001\001\002\001\003\001\004\001\003\001\001\001\
\002\001\003\001\004\001\001\001\002\001\003\001\022\000\014\001\
\019\001\005\001\017\001\005\001\014\001\012\001\030\000\017\001\
\032\000\011\001\006\001\017\001\008\001\015\001\016\001\019\001\
\018\001\019\001\007\001\043\000\009\001\045\000\005\001\006\001\
\048\000\008\001\011\001\010\001\011\001\012\001\013\001\015\001\
\015\001\016\001\012\001\018\001\019\001\005\001\006\001\018\001\
\008\001\016\001\010\001\011\001\012\001\013\001\005\001\015\001\
\016\001\018\000\018\001\019\001\005\001\006\001\255\255\008\001\
\023\000\010\001\011\001\012\001\013\001\255\255\015\001\016\001\
\005\001\018\001\019\001\024\000\025\000\010\001\011\001\012\001\
\013\001\255\255\015\001\016\001\005\001\018\001\019\001\026\000\
\027\000\010\001\011\001\012\001\013\001\255\255\015\001\016\001\
\005\001\018\001\019\001\028\000\029\000\010\001\011\001\012\001\
\013\001\005\001\015\001\016\001\255\255\018\001\019\001\010\001\
\255\255\012\001\013\001\015\001\016\001\255\255\018\001\019\001"

let yynames_const = "\
  LET\000\
  IN\000\
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
# 22 "parser.mly"
                  ( CExp _1 )
# 166 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 23 "parser.mly"
                             ( CDecl (_2, _4) )
# 174 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'var) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 27 "parser.mly"
                                ( ELet(_2,_4,_6) )
# 183 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 28 "parser.mly"
                                ( EIf(_2,_4,_6) )
# 192 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 29 "parser.mly"
                               ( EOr(_1,_3) )
# 200 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 30 "parser.mly"
                                ( _1 )
# 207 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_expr) in
    Obj.repr(
# 34 "parser.mly"
                            ( EAnd(_1,_3) )
# 215 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comp_expr) in
    Obj.repr(
# 35 "parser.mly"
                            ( _1 )
# 222 "parser.ml"
               : 'and_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 39 "parser.mly"
                            ( EEq(_1,_3) )
# 230 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'comp_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 40 "parser.mly"
                            ( ELt(_1,_3) )
# 238 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 41 "parser.mly"
                            ( _1 )
# 245 "parser.ml"
               : 'comp_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 45 "parser.mly"
                                 ( EAdd(_1,_3) )
# 253 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 46 "parser.mly"
                                 ( ESub(_1,_3) )
# 261 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 47 "parser.mly"
                                 ( _1 )
# 268 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 51 "parser.mly"
                                  ( EMul(_1,_3) )
# 276 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 52 "parser.mly"
                                  ( EDiv(_1,_3) )
# 284 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 53 "parser.mly"
                                  ( _1 )
# 291 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 57 "parser.mly"
                   ( EConstInt(_1) )
# 298 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 58 "parser.mly"
                   ( EConstBool(_1) )
# 305 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 59 "parser.mly"
                   ( EVar(_1) )
# 312 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                   ( _2 )
# 319 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
       ( _1 )
# 326 "parser.ml"
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
