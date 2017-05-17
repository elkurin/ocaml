exception Error;;

module type STACK =
  sig
	type 'a t
	val pop 	: 'a t -> 'a * 'a t
	val push 	: 'a -> 'a t -> 'a t
	val empty 	: 'a t
	val size 	: 'a t -> int
  end

module Stack =
  struct
	type 'a t = 'a list
	let pop a = 
	  match a with
	  | [] 	    -> raise Error
	  | x :: xs -> (x, xs)
	let push a s = a :: s
	let empty = []
	let rec size a = 
	  match a with
	  | [] 	    -> 0
	  | x :: xs -> 1 + (size xs)
  end

(*
module Stack =
  struct
	type 'a t = ('a list * int)
	let pop a = 
	  let (b, c) = a in 
	  match b with
	    | [] 	  -> raise Error
	    | x :: xs -> (x, (xs, c - 1))
	let push a s = 
		let (b, c) = s in (a :: b, c + 1)
	let empty = ([], 0)
	let rec size a = let (b, c) = a in c
  end
*)
