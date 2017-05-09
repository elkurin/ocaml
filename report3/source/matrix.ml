exception SizeFail;;

module type SEMIRING =
sig
  type t
  val add : t -> t -> t
  val mul : t -> t -> t
  val unit : t
  val zero : t
end

module Semiring =
struct
  type t = int list list
  let unit = [[1]]
  let zero = [[]]
  let rec add_sub a b =
    match (a, b) with
	| ([[]], [[]]) -> [[]]
	| (x :: xs, y :: ys) -> (x + y) :: (add_sub xs ys)
	| _ -> raise SizeFail
  let rec add a b =
    if a = zero then b
	else if b = zero then a
	else
  	match (a, b) with
	| ([], []) -> []
	| (x :: xs, y :: ys) -> (add_sub x y) :: (add xs ys)
	| _ -> raise SizeFail
  let rec mul_sub a b =
    match (a, b) with
	| ([], []) -> []
	| 
  let rec mul a b =
    if a = zero || b = zero then zero
    else if a = unit then b
	else if b = unit then a
	else
	match (a, b) with
	| ([[]], [[]]) -> [[]]
	| (x :: xs, y :: ys) -> (mul_sub x y) :: (mul xs ys)
	
end
