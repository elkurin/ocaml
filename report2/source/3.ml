let rec fold_left f e l =
	match l with
	| []		-> e
	| x :: xs	-> fold_left f (f e x) xs;;

let reverse l =
	fold_left (fun x y -> y :: x) [] l;;

type 'a tree =
	| Leaf
	| Node of 'a * 'a tree * 'a tree;;


let rec loop q ret =
	match q with 
	| []		-> []
	| (x :: xs) -> (match x with 
					| Leaf -> loop xs ret
					| Node (s, l, r) ->
					loop (xs @ (l :: (r :: []))) (s :: ret));;
let level t = 
	let (_ :: ret) = (loop (t :: []) []) in 
	reverse ret;;

let h = Node (8, Leaf, Leaf);;
let i = Node (9, Leaf, Leaf);;
let d = Node (4, Leaf, Leaf);;
let e = Node (5, h, Leaf);;
let f = Node (6, Leaf, i);;
let g = Node (7, Leaf, Leaf);;
let b = Node (2, d, e);;
let c = Node (3, f, g);;
let a = Node (1, b, c);;
