(* 	ocamlにロードして実行する*)
let sum_to n = (n + 1) * n / 2;;

let rec check p n =
	if p = n then
		true
	else
		if p mod n = 0 then
			false
		else
			check p (n + 1);;
let is_prime p = check p 2;;

let rec gcd a b =
	if a = 0 || b = 0 then
		a + b
	else
		gcd (a mod b) (b mod a);;
