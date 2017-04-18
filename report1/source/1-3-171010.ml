let rec fix f x = f (fix f) x;;

let sum = fun f -> fun n ->
	if n = 0 then
		0
	else
		f (n - 1) + n;;
let sum_to n = fix sum n;;

let prime = fun f -> fun (p, n) ->
	if p = n then
		true
	else
		if p mod n = 0 then
			false
		else
			f (p, (n + 1));;
let is_prime p = fix prime (p, 2);;

let gcd_sub = fun f -> fun (a, b) ->
	if a = 0 || b = 0 then
		a + b
	else
		f ((a mod b), (b mod a));;
let gcd a b = fix gcd_sub (a, b);;
