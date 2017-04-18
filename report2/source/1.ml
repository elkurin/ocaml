type nat = Z | S of nat;;

let add a b =
	match a with
	| Z		-> b
	| S x	-> match b with
				| Z		-> x
				| S y	-> x +. y;;
