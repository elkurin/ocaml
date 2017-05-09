type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree;;

type order = LT | EQ | GT

module type ORDERED_TYPE = 
sig 
  type t
  val compare : t -> t -> order 
end

module OrderedInt =
struct 
  type t = int
  let compare a b =
  	if a < b then LT
	else if a = b then EQ
	else GT
end

module type MULTISET2 = 
  functor (T : ORDERED_TYPE) -> 
    sig 
      type t 
      val empty : t 
      val add    : T.t -> t -> t 
      val remove : T.t -> t -> t 
      val count  : T.t -> t -> int 
    end 

module Multiset2 : MULTISET2 =
  functor (T : ORDERED_TYPE) -> struct
  	type t = T.t tree
	let rec add a xs =
	  match xs with
	  | Leaf -> Node (a, Leaf, Leaf)
	  | Node (s,l,r) -> 
	    (match T.compare a s with
		  | LT -> Node (s, (add a l), r)
		  | _  -> Node (s, l, (add a r)))
	let rec merge l r =
	  match r with
	  | Leaf -> l
	  | Node (s,x,y) -> Node (s, (merge l x), y)
	let rec remove a xs =
	  match xs with
	  | Leaf -> Leaf
	  | Node (s,l,r) ->
	  	(match T.compare a s with
		  | LT -> Node (s, (remove a l), r)
		  | EQ -> (match l with 
		  		| Leaf -> r
		  		| Node (x,y,z) ->
		  		  Node (x, y, (merge r z)))
		  | GT -> Node (s, l, (remove a r)))
	let rec count a xs =
		match xs with
		| Leaf -> 0
		| Node (s,l,r) ->
		  (match T.compare a s with
		    | LT -> count a l
			| EQ -> 1 + (count a r)
			| GT -> count a r)
	let empty = Leaf
  end

module Btree = Multiset2 (OrderedInt)
