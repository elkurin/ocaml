type name = string 

type expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name 
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EAnd       of expr * expr
  | EOr        of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr		 
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | EFun       of name * expr
			  
type value =
  | VInt  of int
  | VBool of bool
  | VFun  of name * expr * env
and env = (name * value) list

type command =
  | CExp  of expr
  | CDecl of name * expr
  | DDecl of name * expr * command
  | NDecl of name * expr * command
  | DLai  of name * expr * command * expr
				  
let print_name = print_string 

let print_value v =
  match v with
  | VInt i -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun (x, y, z) -> print_string x

(*
 小さい式に対しては以下でも問題はないが，
 大きいサイズの式を見やすく表示したければ，Formatモジュール
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
 を活用すること
*)
let rec print_expr e =
  match e with
  | EConstInt i ->
     print_int i
  | EConstBool b ->
     print_string (string_of_bool b)
  | EVar x -> 
     print_name x
  | EAdd (e1,e2) -> 
     (print_string "EAdd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ESub (e1,e2) -> 
     (print_string "ESub (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EMul (e1,e2) -> 
     (print_string "EMul (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EDiv (e1,e2) -> 
     (print_string "EDiv (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EAnd (e1,e2) -> 
     (print_string "EAnd (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EOr (e1,e2) -> 
     (print_string "EOr (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EEq (e1,e2) ->
     (print_string "EEq (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ELt (e1, e2) ->
     (print_string "ELt (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EIf (e1,e2,e3) ->
     (print_string "EIf (";
      print_expr   e1;
      print_string ","; 
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")")
  | ELet (e1,e2,e3) ->
     (print_string "ELet (";
      print_string e1;
      print_string ","; 
      print_expr   e2;
      print_string ",";
      print_expr   e3;
      print_string ")")
  | EFun (e1, e2) ->
     (print_string "EFun (";
      print_string e1;
      print_string ",";
      print_expr e2;
      print_string ")")

let rec print_command p =       
  match p with
  | CExp e -> print_expr e
  | CDecl (e1, e2) -> print_string "CDecl ("; 
                      print_string e1;
                      print_string "=";
                      print_expr   e2;
                      print_string ")"
  | DDecl (e1, e2, e3) -> print_string "DDecl ("; 
                          print_string e1;
                          print_string "=";
                          print_expr   e2;
                          print_string ")"
  | NDecl (e1, e2, e3) -> print_string "NDecl ("; 
                          print_string e1;
                          print_string "=";
                          print_expr   e2;
                          print_string ")"
  | DLai (e1, e2, e3, e4)  -> print_string "NLai ("; 
                              print_string e1;
                              print_string "=";
                              print_expr   e2;
                              print_string ")"
