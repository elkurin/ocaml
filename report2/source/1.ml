type nat = Z | S of nat;;

let a = Z;;
let b = S (S (S (S Z)));;
let c = S (S (S Z));;

let rec add a b =
	match a with
	| Z 	-> b
	| S x	-> S (add x b);;

let rec sub a b =
	match a with
	| Z		-> Z
	| S x	-> (match b with
				| Z		-> S (sub x Z)
				| S y	-> (sub x y));;


let rec mul a b =
	match b with
	| Z		-> Z
	| S x	-> add a (mul a x);;

let rec pow a b =
	match b with
	| Z		-> (S Z)
	| S x	-> mul a (pow a x);;

let rec n2i a =
	match a with
	| Z		-> 0
	| S x	-> (n2i x) + 1;;

let rec i2n a =
	if a = 0 then Z else S (i2n (a - 1));;
