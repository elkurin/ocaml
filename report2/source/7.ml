type false_t = {t : 'a. 'a};;
type 'a not_t = 'a -> false_t;;
type ('a, 'b) and_t = 'a * 'b;;
type ('a, 'b) or_t = L of 'a | R of 'b;;
(*exception Exception;;*)

let rec callcc : (('a -> false_t) -> 'a) -> 'a = fun f -> callcc f;;

(*1. ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c) *)
let func1 f g x = g (f x);;

(*2. ('a, ('b, 'c) and_t) or_t ->
		(('a, 'b) or_t, ('a, 'c) or_t) and_t *)
let func2 (x : ('a, ('b, 'c) and_t) or_t) = 
	match x with
	| L l 		-> ((L l, L l) : ((_, _) and_t))
	| R (b, c)	-> ((R b, R c) : ((_, _) and_t));;
(*
もしand_tが type ('a, 'b) and_t = P of 'a * 'b と定義されて入れば以下のように書ける

let func2 x = 
	match x with
	| L l -> P (L l, L l)
	| R P (b, c) -> P (R b, R c);;
*)

(*3. (('a, 'b) or_t, ('a, 'c) or_t) and_t ->
		('a, ('b, 'c) and_t) or_t *)
let func3 (x : (('a, 'b) or_t, ('a, 'c) or_t) and_t) =
	match x with
	| (R b, R c) -> R ((b, c) : ('b, 'c) and_t)
	| (R _, L a) -> L a
	| (L a, _)	 -> L a;;


(*4. ('a, 'a not_t) or_t 
直観主義では証明不可能なのでcallccがないと無理*)
let p x = (fun (y : 'a) -> x (L y) : 'a not_t);;
let q = fun (x : ('a, 'a not_t) or_t not_t) -> R (p x);;
let func4 (f : 'a -> 'c) (g : 'a not_t -> 'c) =
	match callcc q with
	| L a -> f a
	| R a -> g a;;

(*4はこれで良かったっぽい*)
let f4 g h =
	match (callcc (fun q -> R (fun x -> q (L x)))) with
	| L a -> g a
	| R a -> h a;;

(*
5. ('a, 'a not_t) and_t
は誤りなのでこれを満たす型は作れない
*)

(*6. (('a -> 'b) -> 'a) -> 'a 
直観主義では証明不可能なのでcallccがないと無理*)
let (a : 'a not_t not_t -> 'a not_t -> 'a) = fun x -> fun y -> (x y).t;;
let notnot (x : 'a not_t not_t) = callcc (a x);; (*二重否定除去則*)
let b (y : 'a not_t) (x : 'a) =  (y x).t;;
let c (z : ('a -> 'b) -> 'a) (y : 'a not_t) = y (z (b y));;
let func6 (z : ('a -> 'b) -> 'a) = notnot (c z);;

