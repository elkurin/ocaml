let rec fold_right f l e =
	match l with 
	| [] 		-> e
	| x :: xs	-> f x (fold_right f xs e);;

let rec fold_left f e l =
	match l with
	| []		-> e
	| x :: xs	-> fold_left f (f e x) xs;;

let reverse l =
	fold_left (fun x y -> y :: x) [] l;;

let fold_left_r f e l =
	(fold_right (fun x g -> (fun h -> g (f h x))) l (fun h -> h)) e;;
let reverse_right l =
	fold_left_r (fun x y -> y :: x) [] l;;
