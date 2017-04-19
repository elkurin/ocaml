let rec fold_left f e l =
	match l with
	| []		-> e
	| x :: xs	-> fold_left f (f e x) xs;;

let reverse l =
	fold_left (fun x y -> y :: x) [] l;;

type 'a tree =
	| Leaf
	| Node of 'a * 'a tree * 'a tree;;


let rec pre t =
	match t with
	| Leaf			-> []
	| Node (s,l,r)	-> s :: ((pre l) @ (pre r));;

let rec ino t =
	match t with
	| Leaf 			-> []
	| Node (s,l,r)	-> (ino l) @ (s :: (ino r));;

let rec post_sub t =
	match t with
	| Leaf			-> []
	| Node (s,l,r)	-> s :: ((post_sub r) @ (post_sub l));;

let post t = reverse (post_sub t);;

let d = Node (4, Leaf, Leaf);;
let e = Node (5, Leaf, Leaf);;
let f = Node (6, Leaf, Leaf);;
let g = Node (7, Leaf, Leaf);;
let b = Node (2, d, e);;
let c = Node (3, f, g);;
let a = Node (1, b, c);;
