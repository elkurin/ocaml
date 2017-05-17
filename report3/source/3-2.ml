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
  functor (T: ORDERED_TYPE) -> struct
    type t = (T.t * int) list
    let rec add a l =
      match l with
      | [] -> (a, 1) :: []
      | (x, y) :: xs -> 
          (match T.compare a x with
          | LT -> (a, 1) :: l
          | EQ -> (a, y + 1) :: xs
          | GT -> (x, y) :: (add a xs))
    let rec remove a l =
      match l with 
      | [] -> []
      | (x, y) :: xs ->
          (match T.compare a x with
          | LT -> l
          | EQ -> if y <= 1 then xs else (x, y - 1) :: xs
          | GT -> (x, y) :: (remove a xs))
    let rec count a l =
      match l with
      | [] -> 0
      | (x, y) :: xs ->
          (match T.compare a x with
          | LT -> 0
          | EQ -> y
          | GT -> count a xs)
    let empty = []
  end

module Bucket = Multiset2 (OrderedInt)
