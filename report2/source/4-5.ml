type value = VInt of int | VBool of bool;;

exception Eval_error;;

type expr =
	| EConstInt	 of int
	| EAdd		 of expr * expr
	| ESub		 of expr * expr
	| EMul		 of expr * expr
	| EDiv		 of expr * expr
	| EConstBool of bool
	| EEq		 of expr * expr
	| ELt		 of expr * expr
	| EIfElse	 of expr * expr * expr;;

let addV a b =
	match (a, b) with
	| (VInt x, VInt y)	-> VInt (x + y)
	| (VBool x, VBool y)-> VBool (not (x && y))
	| _					-> raise Eval_error;;

let subV a b =
	match (a, b) with
	| (VInt x, VInt y)	-> VInt (x - y)
	| (VBool x, VBool y)-> VBool (not (x && y))
	| _					-> raise Eval_error;;

let mulV a b =
	match (a, b) with
	| (VInt x, VInt y)	-> VInt (x * y)
	| (VBool x, VBool y)-> VBool ((x = true) && (y = true))
	| _					-> raise Eval_error;;

let divV a b =
	match (a, b) with
	| (VInt x, VInt y) 	-> VInt (x / y)
	| _					-> raise Eval_error;;

let eqV a b =
	match (a, b) with
	| (VInt x, VInt y)	-> VBool (x = y)
	| (VBool x, VBool y)-> VBool (x = y)
	| _					-> raise Eval_error;;

let ltV a b =
	match (a, b) with
	| (VInt x, VInt y) 	-> VBool (x < y)
	| _					-> raise Eval_error;;

let ifV c a b =
	match c with
	| VInt  x -> raise Eval_error
	| VBool x -> if x then a else b;;

let rec eval a =
	match a with
	| EConstInt	x		-> VInt x
	| EAdd (x, y)		-> addV (eval x) (eval y)
	| ESub (x, y)		-> subV (eval x) (eval y)
	| EMul (x, y)		-> mulV (eval x) (eval y)
	| EDiv (x, y)		-> divV (eval x) (eval y)
	| EConstBool x		-> VBool x
	| EEq (x, y)		-> eqV (eval x) (eval y)
	| ELt (x, y)		-> ltV (eval x) (eval y)
	| EIfElse (c, x, y) -> ifV (eval c) (eval x) (eval y);;

let a = EConstInt 4;;
let b = EConstInt 2;;
let t = EConstBool true;;
let f = EConstBool false;;
let c = EAdd (a, b);;
let d = ESub (a, b);;
let e = EMul (a, b);;
let g = EAdd (t, f);;
let h = EMul (t, f);;
let i = EAdd (a, t);;
let j = EEq (t, f);;
let k = EEq (a, b);;
let l = ELt (a, b);;
let m = EIfElse (t, a, b);;

eval a;;
eval t;;
eval c;;
eval d;;
eval e;;
eval g;;
eval h;;
eval j;;
eval k;;
eval l;;
eval m;;
eval i;;

