exception SizeFail;;

module type SEMIRING =
sig
  type t
  val add : t -> t -> t
  val mul : t -> t -> t
  val unit : t
  val zero : t
end

module type MATRIX =
  functor (T: SEMIRING) ->
  	sig
	  type t
	  val add : t -> t -> t
	  val mul : t -> t -> t
	end

module Matrix =
  functor (T: SEMIRING) ->
	struct
	  type t = T.t list list
	  let rec add_sub a b =
		match (a, b) with
		| ([], []) -> []
		| (x :: xs, y :: ys) -> (T.add x y) :: (add_sub xs ys)
		| _ -> raise SizeFail
	  let rec add a b =
		match (a, b) with
		| ([], []) -> []
		| (x :: xs, y :: ys) -> (add_sub x y) :: (add xs ys)
		| _ -> raise SizeFail
	  let rec transport_sub x c =
		match x with
		| [] -> c
		| y :: ys -> (match c with 
					| [] -> [y] ::  (transport_sub ys [])
					| z :: zs -> (z @ (y :: [])) :: (transport_sub ys zs))
	  let rec transport b c =
		match b with
		| [] -> c
		| x :: xs -> transport xs (transport_sub x c)
	  let rec mul_sub3 l r =
		match (l, r) with
		| ([], []) -> T.zero
		| (x :: xs, y :: ys) -> T.add (T.mul x y) (mul_sub3 xs ys)
		| _ -> raise SizeFail
	  let rec mul_sub2 l c =
		match c with
		| [] -> []
		| x :: xs -> (mul_sub3 l x) :: (mul_sub2 l xs)
	  let rec mul_sub1 a c =
		match a with
		| [] -> []
		| x :: xs -> (mul_sub2 x c) :: (mul_sub1 xs c)
	  let rec mul a b =
		mul_sub1 a (transport b [])
	
end

module SemiringBool =
struct
  type t = bool
  let add x y = if x = false && y = false then false else true
  let mul x y = if x = true && y = true then true else false
  let unit = true
  let zero = false
end

module SemiringMin =
struct
  type t = Int of int | Inf
  let add x y =
  	match (x, y) with
	| (Int a, Int b) -> if a < b then x else y
	| (Inf, Int b) -> y
	| (Int a, Inf) -> x
	| _ -> Inf
  let mul x y =
  	match (x, y) with
	| (Int a, Int b) -> Int (a + b)
	| _ ->  Inf
  let unit = Int 1
  let zero = Int 0
end

module SemiringInt =
struct
  type t = int
  let add x y = x + y
  let mul x y = x * y
  let unit = 1
  let zero = 0
end

module BoolMatrix = Matrix (SemiringBool)
module MinMatrix = Matrix (SemiringMin)
module IntMatrix = Matrix (SemiringInt)
