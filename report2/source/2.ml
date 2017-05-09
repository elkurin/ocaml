let rec fold_left f e l =
	match l with
	| []		-> e
	| x :: xs	-> fold_left f (f e x) xs;;

let reverse l =
	fold_left (fun x y -> y :: x) [] l;;

type 'a tree =
	| Leaf
	| Node of 'a * 'a tree * 'a tree;;

let rec pre_order a t =
	match a with
	| Leaf			-> t
	| Node (s,l,r)	-> pre_order r (pre_order l (s :: t));;
let pre t = reverse (pre_order t []);;

let rec in_order a t =
	match a with
	| Leaf			-> t
	| Node (s,l,r)	-> in_order r (s :: (in_order l t));;
let ino t = reverse (in_order t []);;

let rec post_order a t =
	match a with
	| Leaf			-> t
	| Node (s,l,r)	-> s :: (post_order r (post_order l t));;
let post t = reverse (post_order t []);;


let h = Node (8, Leaf, Leaf);;
let i = Node (9, Leaf, Leaf);;
let d = Node (4, Leaf, Leaf);;
let e = Node (5, h, Leaf);;
let f = Node (6, Leaf, Leaf);;
let g = Node (7, Leaf, i);;
let b = Node (2, d, e);;
let c = Node (3, f, g);;
let a = Node (1, b, c);;
