let rec fold_right f l e =
	match l with 
	| [] 		-> e
	| x :: xs	-> f x (fold_right f xs e);;

let rec fold_left f e l =
	match l with
	| []		-> e
	| x :: xs	-> fold_left f (f e x) xs;;

let append_left a b =
	fold_left (fun x y -> y :: x) b (fold_left (fun x y -> y :: x) [] a);;

let filter_left c a =
	fold_left (fun x y -> y :: x) [] (fold_left (fun x y -> if c y then y :: x else x) [] a);;

let append_right a b =
	fold_right (fun x y -> x :: y) a b;;

let filter_right c a =
	fold_right (fun x y -> if c x then x :: y else y) a [];;
