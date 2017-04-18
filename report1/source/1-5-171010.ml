let rec append left right =
	match left with
	| []		-> right
	| x :: xs	-> x :: (append xs right)

let rec filter con l =
	match l with
	| []		-> []
	| x :: xs	-> 
		if con x then
			x :: (filter con xs)
		else
			filter con xs;;
