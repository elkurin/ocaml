type token =
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)
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
  | SEMISEMI

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Syntax
  (* ここに書いたものは，ExampleParser.mliに入らないので注意 *)
# 24 "parser.ml"
let yytransl_const = [|
  260 (* PLUS *);
  261 (* TIMES *);
  262 (* MINUS *);
  263 (* DIV *);
  264 (* EQ *);
  265 (* LT *);
  266 (* IF *);
  267 (* THEN *);
  268 (* ELSE *);
  269 (* LPAR *);
  270 (* RPAR *);
  271 (* SEMISEMI *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* BOOL *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\003\000\003\000\003\000\
\004\000\004\000\004\000\005\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\002\000\006\000\003\000\003\000\001\000\003\000\003\000\001\000\
\003\000\003\000\001\000\001\000\001\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\013\000\014\000\000\000\000\000\016\000\
\000\000\000\000\000\000\011\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\000\000\000\000\000\000\009\000\010\000\000\000\000\000\002\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\011\000\012\000"

let yysindex = "\007\000\
\001\255\000\000\000\000\000\000\000\000\001\255\001\255\000\000\
\000\255\051\255\013\255\000\000\011\255\014\255\000\000\004\255\
\004\255\004\255\004\255\004\255\004\255\001\255\000\000\013\255\
\013\255\028\255\028\255\000\000\000\000\025\255\001\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\254\254\015\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\255\
\039\255\050\255\055\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\250\255\053\000\057\000\055\000"

let yytablesize = 76
let yytable = "\013\000\
\014\000\003\000\004\000\005\000\003\000\004\000\005\000\001\000\
\005\000\005\000\006\000\005\000\005\000\007\000\015\000\030\000\
\007\000\020\000\008\000\021\000\008\000\022\000\008\000\008\000\
\032\000\008\000\008\000\023\000\008\000\008\000\006\000\016\000\
\006\000\017\000\006\000\006\000\031\000\006\000\006\000\000\000\
\006\000\006\000\007\000\000\000\007\000\000\000\007\000\007\000\
\000\000\007\000\007\000\000\000\007\000\007\000\016\000\000\000\
\017\000\000\000\018\000\019\000\003\000\003\000\000\000\003\000\
\003\000\004\000\004\000\000\000\004\000\004\000\026\000\027\000\
\024\000\025\000\028\000\029\000"

let yycheck = "\006\000\
\007\000\001\001\002\001\003\001\001\001\002\001\003\001\001\000\
\011\001\012\001\010\001\014\001\015\001\013\001\015\001\022\000\
\013\001\005\001\004\001\007\001\006\001\011\001\008\001\009\001\
\031\000\011\001\012\001\014\001\014\001\015\001\004\001\004\001\
\006\001\006\001\008\001\009\001\012\001\011\001\012\001\255\255\
\014\001\015\001\004\001\255\255\006\001\255\255\008\001\009\001\
\255\255\011\001\012\001\255\255\014\001\015\001\004\001\255\255\
\006\001\255\255\008\001\009\001\011\001\012\001\255\255\014\001\
\015\001\011\001\012\001\255\255\014\001\015\001\018\000\019\000\
\016\000\017\000\020\000\021\000"

let yynames_const = "\
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
# 20 "parser.mly"
                  ( CExp _1 )
# 131 "parser.ml"
               : Syntax.command))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 24 "parser.mly"
                                ( EIf(_2,_4,_6) )
# 140 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 25 "parser.mly"
                                ( EEq(_1,_3) )
# 148 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 26 "parser.mly"
                                ( ELt(_1,_3) )
# 156 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith_expr) in
    Obj.repr(
# 27 "parser.mly"
                                ( _1 )
# 163 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 31 "parser.mly"
                                 ( EAdd(_1,_3) )
# 171 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 32 "parser.mly"
                                 ( ESub(_1,_3) )
# 179 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor_expr) in
    Obj.repr(
# 33 "parser.mly"
                                 ( _1 )
# 186 "parser.ml"
               : 'arith_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 37 "parser.mly"
                                  ( EMul(_1,_3) )
# 194 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'factor_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 38 "parser.mly"
                                  ( EDiv(_1,_3) )
# 202 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atomic_expr) in
    Obj.repr(
# 39 "parser.mly"
                                  ( _1 )
# 209 "parser.ml"
               : 'factor_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 43 "parser.mly"
                   ( EConstInt(_1) )
# 216 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 44 "parser.mly"
                   ( EConstBool(_1) )
# 223 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                   ( EVar(_1) )
# 230 "parser.ml"
               : 'atomic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 46 "parser.mly"
                   ( _2 )
# 237 "parser.ml"
               : 'atomic_expr))
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
