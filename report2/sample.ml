(*-----例題1-----*)
type complex =
	{re : float; im : float};;

let prod a b =
	{re =  a.re *. b.re -. a.im *. b.im; im = a.re *. b.im +. a.im *. b.re};;
let ci = {re = 1.0; im = 2.0};;
prod ci ci;;

type str_tree =
	| Leaf
	| Node of string * str_tree * str_tree;;

let l = Node ("on", Leaf, Leaf);;
let r = Node ("Titan", Leaf, Leaf);;
let x = Node ("Attack", l, r);;

type ib_list = INil
			 | Icons of int * bi_list
and  bi_list = BNil
			 | Bcons of bool * ib_list;;

let a = Icons (1, BNil);;
let b = Bcons (true, INil);;
let c = Icons (0, b);;
let d = Bcons (false, c);;
let e = Icons (5, d);;

(*-----例題2-----*)
type iexpr =
	| EConstInt of int
	| EAdd		of iexpr * iexpr
	| ESub		of iexpr * iexpr
	| EMul		of iexpr * iexpr;;
let rec eval a =
	match a with
	| EConstInt x 	-> x
	| EAdd (x, y)	-> (eval x) + (eval y)
	| ESub (x, y)	-> (eval x) - (eval y)
	| EMul (x, y)	-> (eval x) * (eval y);;
let e1 = (EAdd (EConstInt 3, EConstInt 4));;
let e2 = (EMul (e1, EConstInt 8));;
eval (ESub (e2, EConstInt 6));;

(*-----例題3-----*)
let l = ref (0, 0);;
let f x =
	l := (let (n, _) = !l in (x, n));
	let (n, p) = !l in p;;
f 10;;
f (- 5);;
f 3;;
