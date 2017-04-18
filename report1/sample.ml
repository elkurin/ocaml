let circle r = r *. r *. 3.141592;;
circle 10.0;;
circle 15.0;;


let rec sigma f n =
	if n == 0 then
		f 0
	else
		(f n) + (sigma f (n - 1));;
sigma (fun x -> x * x + x) 10;;

let rec map f l =
	match l with
	| []		-> []
	| car :: cdr-> (f car) :: (map f cdr);;
map (fun x -> x * x) [1; 2; 3];;
