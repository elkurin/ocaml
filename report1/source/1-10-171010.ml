let rec g n f x =
	if n = 0 then x else f (g (n - 1) f x);;

let add n m = fun f x -> g n f (g m f x);;
let mul n m = fun f x -> g n (g m f) x

let max a b =
	if a > b then a else b;;
let sub n m = fun f x -> g (max 0 (n - m)) f x;;
