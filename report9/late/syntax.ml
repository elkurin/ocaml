type name = string 

type pattern =
  | PInt  of int
  | PBool of bool
  | PVar  of name
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern
	       
type expr =
  | EConstInt  of int
  | EConstBool of bool
  | EVar       of name 
  | EAdd       of expr * expr
  | ESub       of expr * expr
  | EMul       of expr * expr
  | EDiv       of expr * expr
  | EEq        of expr * expr
  | ELt        of expr * expr		 
  | EIf        of expr * expr * expr
  | ELet       of name * expr * expr
  | ELetFun    of name list * expr * expr
  | EFun       of name * expr
  | EFuns      of name list * expr
  | ERecFun    of name * name * expr
  | EDFun      of name * expr
  | EApp       of expr * expr
  | ELetRec    of name * name * expr * expr
  | EPair      of expr * expr
  | ECons      of expr * expr
  | ENil
  | EMatch     of expr * (pattern * expr) list

type command =
  | CExp     of expr
  | CDecl    of name * expr
  | CFunDecl of name list * expr
  | CRecDecl of name * name * expr
				  
let print_name = print_string 

let rec print_pattern p =
  match p with
  | PInt i -> print_int i
  | PBool b -> print_string (string_of_bool b)
  | PVar x -> print_string x
  | PPair (p1, p2) ->
     (print_string "(";
      print_pattern p1;
      print_string ",";
      print_pattern p2;
      print_string ")")
  | PNil -> print_string "[]"
  | PCons (p1, p2) ->
     (print_pattern p1;
      print_string "::";
      print_pattern p2)
    
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
  | EPair (e1, e2) ->
     (print_string "EPair (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ENil ->
     print_string "ENil"
  | ECons (e1, e2) ->
     (print_string "ECons (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
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
  | ELet (x,e1,e2) ->
     (print_string ("ELet (" ^ x ^ ",");
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | EFun (x,e) ->
     (print_string ("EFun (" ^ x ^ ",");
      print_expr e;
      print_string ")")
  | EFuns (x,e) ->
     (print_string ("EFuns (");
      print_expr e;
      print_string ")")
  | ERecFun (f,x,e) ->
     (print_string ("ERecFun (" ^ x ^ ",");
      print_expr e;
      print_string ")")
  | EDFun (x,e) ->
     (print_string ("EDFun (" ^ x ^ ",");
      print_expr e;
      print_string ")")
  | EApp (e1,e2) ->
     (print_string "EApp (";
      print_expr e1;
      print_string ",";
      print_expr e2;
      print_string ")")
  | ELetRec (f,x,e1,e2) ->
     (print_string ("ELetRec ([");
      print_string "],";
      print_expr e2;
      print_string ")")
  | ELetFun (l,e1,e2) ->
     (print_string ("ELetFun"))
  | EMatch (e, cases) ->
     (print_string "EMatch (";
      print_expr e;
      print_string ",";
      print_cases cases;
      print_string ")")
and print_cases cases =
  List.iter (fun (p, e) ->
	     print_pattern p;
	     print_string " -> ";
	     print_expr e;
	     print_string ",")
	    cases
		   
       
let rec print_command p =       
  match p with
  | CExp e -> print_expr e
  | CDecl (x,e) ->
     (print_string ("CDecl (" ^ x ^ ",");
      print_expr e;
      print_string ")")
  | CFunDecl (l, e) -> 
     (print_string ("CFunDecl"))
  | CRecDecl (f,x,e) ->
     (print_string ("CRecDecl ([");
      print_string "])")
