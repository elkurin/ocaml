type ('a, 'b) tree =
  | Leaf
  | Node of 'a * 'b * ('a, 'b) tree * ('a, 'b) tree;;

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

module type MAP =
  functor (T : ORDERED_TYPE) ->
    sig
	  type 'a t
	  val empty  : 'a t
	  val add    : T.t -> 'a -> 'a t -> 'a t
	  val remove : T.t -> 'a t -> 'a t
	  val lookup : T.t -> 'a t -> 'a -> 'a
	end

module Map : MAP =
  functor (T : ORDERED_TYPE) -> struct
  	type 'a t = (T.t, 'a) tree
	let empty = Leaf
	let rec add key value t =
		match t with
		| Leaf -> Node (key, value, Leaf, Leaf)
		| Node (k,v,l,r) ->
			(match T.compare key k with
			| LT -> Node (k,v,(add key value l),r)
			| EQ -> Node (k,value,l,r)
			| GT -> Node (k,v,l,(add key value r)))
	let rec merge l r =
		match r with
		| Leaf -> l
		| Node (k,v, x, y) -> Node (k,v, (merge l x), y)
	let rec remove key t =
		match t with
		| Leaf -> Leaf
		| Node (k,v,l,r) ->
			(match T.compare key k with
			  | LT -> Node (k,v, (remove key l), r)
			  | EQ -> (match l with 
					| Leaf -> r
					| Node (p,q,y,z) ->
					  Node (p,q, y, (merge r z)))
			  | GT -> Node (k, v, l, (remove key r)))
	(* 引数にkey, t, nullをとる ここでnullは、keyが見つからなかった時に返してほしい値 *)
	let rec lookup key t null =
		match t with
		| Leaf -> null
		| Node (k,v,l,r) ->
			(match T.compare key k with
			| LT -> lookup key l null
			| EQ -> v
			| GT -> lookup key r null)
  end


module MapInt = Map (OrderedInt)
