let rec fold_right f l e =
	match l with 
	| [] 		-> e
	| x :: xs	-> f x (fold_right f xs e);;

let rec fold_left f e l =
	match l with
	| []		-> e
	| x :: xs	-> fold_left f (f e x) xs;;

let fold_right_l f l e =
	(fold_left (fun g x -> (fun h -> g (f x h))) (fun h -> h) l) e;;
let append_right a b =
	fold_right_l (fun x y -> x :: y) a b;;

let fold_left_r f e l =
	(fold_right (fun x g -> (fun h -> g (f h x))) l (fun h -> h)) e;;
let append_left a b =
	fold_left_r (fun x y -> y :: x) b (fold_left (fun x y -> y :: x) [] a);;

