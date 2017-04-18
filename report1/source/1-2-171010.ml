let twice f = 
	fun x -> f (f x);;

let rec repeat f n =
	if n = 1 then
		fun x -> f x
	else
		fun x -> f (repeat f (n - 1) x);;
