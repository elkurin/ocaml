type 'a m = 'a list;;
let (>>=) x f = List.concat (List.map f x);;
let return x = [x];;
let guard b = if b then return () else [];;

let sat f = 
	[true; false] >>= (fun x ->
	[true; false] >>= (fun y ->
	[true; false] >>= (fun z ->
	(guard (f x y z)) >>= (fun _ ->
		return (x, y, z)))));;

let f x y z = if x && y && z then true else if x=false && y=false && z=false then true else false;;
let g x y z = if (x && y) || (y && z) || (z && x) then true else false;;

sat f;;
sat g;;
