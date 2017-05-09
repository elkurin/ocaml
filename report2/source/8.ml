let curry f x y = f (x, y);;
let uncurry f (x, y) = f x y;;

exception BlackBox;;
let f = fun z -> raise BlackBox;;
let h = fun g -> fun (x, y) -> g (x, y);;

