let rec fold_left f e l =
	match l with
	| []		-> e
	| x :: xs	-> fold_left f (f e x) xs;;

let reverse l =
	fold_left (fun x y -> y :: x) [] l;;

type 'a tree =
	| Leaf
	| Node of 'a * 'a tree * 'a tree;;

(* -----解法1----- *)
type 'a mlist = MNil | MCons of 'a * 'a mlist ref;;
type 'a queue = {mutable cur : 'a mlist; mutable last : 'a mlist};;
let push a q =
	let c = MCons (a, ref MNil) in
	match q.last with
	| MNil				-> q.cur <- c; q.last <- c; q
	| MCons (_, next) 	-> next := c; q.last <- c; q;;
let pop q = 
	match q.cur with
	| MNil 				-> q
	| MCons (_, next) 	-> q.cur <- !next; q;;

let ret = ref [];;
let rec loop q =
	match q.cur with
	| MNil			-> ()
	| MCons (t, _)	-> (match t with
						| Leaf 			-> loop (pop q)
						| Node (s,l,r)  ->
							ret := (s :: !ret);
							loop (pop (push r (push l q))));;
let level t = 
	let q : 'a tree queue = {cur = MNil; last = MNil} in loop (push t q); 
	reverse !ret;;

(* -----解法2----- *)
let rec func t d q =
	match t with
	| Leaf			-> q
	| Node (s,l,r)	-> func r (d + 1) (func l (d + 1) ((s, d) :: q));;

let rec filter_def d l def =
	match l with
	| []		-> def
	| x :: xs	-> 
		let (s, i) = x in
		if d = i then
			s :: (filter_def d xs def)
		else
			filter_def d xs def;;

let rec lev d l ret =
	match (filter_def d l ret) with
	| []		-> []
	| x :: xs	-> (match ret with 
					| [] 		-> lev (d + 1) l (x :: xs)
					| y :: ys 	-> (if x = y then x :: xs else lev (d + 1) l (x :: xs)));;

let level2 t = reverse (lev 0 (func t 0 []) []);;

(* -----解法3----- *)
let rec bunkers l r =
	match l with
	| []					-> if r = [] then [] else bunkers (reverse r) []
	| Node (s,a,b) :: xs	-> s :: (bunkers xs (b :: a :: r))
	| Leaf :: xs			-> bunkers xs r;;
let level3 t = bunkers [] (t :: []);;

(* -----テスト用----- *)
let h = Node (8, Leaf, Leaf);;
let i = Node (9, Leaf, Leaf);;
let d = Node (4, Leaf, Leaf);;
let e = Node (5, h, Leaf);;
let f = Node (6, Leaf, i);;
let g = Node (7, Leaf, Leaf);;
let b = Node (2, d, e);;
let c = Node (3, f, g);;
let a = Node (1, b, c);;
