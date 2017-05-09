exception White_magic;;
let g = ref (fun x -> raise White_magic);;
let fix f = g := (fun x -> f !g x); !g;;


(*-----テスト用-----*)
(*
let sum = fun f -> fun n ->
	if n = 0 then
		0
	else
		f (n - 1) + n;;
let sum_to n = fix sum n;;
*)
let prime = fun f -> fun p -> fun n ->
	if p = n then
		true
	else
		if p mod n = 0 then
			false
		else
			f p (n + 1);;
let is_prime p = fix prime p 2;;
