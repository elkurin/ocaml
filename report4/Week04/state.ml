type 'a int_state = int -> ('a * int)
let addVal x = (fun i -> ((), i + x))
let getVal () = (fun i -> (i, i))
let (>>=) x f = fun init -> let (r, s) = x init in f r s
let return x = fun init -> (x, init)

let addAddGetAddval =
	addVal 3 >>= (fun _ ->
	addVal 7 >>= (fun _ ->
	getVal () >>= (fun x ->
	addVal x)))
