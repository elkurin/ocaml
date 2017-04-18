let rec add x ll =
	match ll with
	| []		-> (x :: []) :: []
	| y :: ys	-> (x :: y) :: (add x ys);;

let rec func l r =
	match r with
	| []		-> []
	| x :: xs	-> (add x (func [] (l @ xs))) @ (func (l @ (x :: [])) xs);;

let rec size l =
	match l with
	| []		-> 0
	| x :: xs	-> (size xs) + 1;;

let rec filter con l =
	match l with
	| []		-> []
	| x :: xs	-> 
		if con x then
			x :: (filter con xs)
		else
			filter con xs;;

let perm l =
	let s = size l in
	filter (fun x -> s = (size x)) (func [] l);;
