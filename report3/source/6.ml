module type EQ = 
sig
  type ('a, 'b) equal
  val refl : ('a, 'a) equal
  val symm : ('a, 'b) equal -> ('b, 'a) equal
  val trans : ('a, 'b) equal -> ('b, 'c) equal -> ('a, 'c) equal
  val apply : ('a, 'b) equal -> 'a -> 'b
end

module Eq =
struct
  type ('a, 'b) equal = (('a -> 'b) * ('b -> 'a))
  let refl = ((fun x -> x : 'a -> 'a), (fun x -> x : 'a -> 'a))
  let symm x = let (a, b) = x in (b, a)
  let trans x y = let (a, b) = x in let (c, d) = y in  (fun x -> c (a x), fun x -> b (d x))
  let apply x y = let (a, _) = x in a y
end

type 'a value =
  | VBool of (bool, 'a) Eq.equal * bool
  | VInt of (int, 'a) Eq.equal * int
type 'a expr =
  | EConstInt of (int, 'a) Eq.equal * int
  | EConstBool of (bool, 'a) Eq.equal * bool
  | EAdd of (int, 'a) Eq.equal * int expr * int expr
  | ESub of (int, 'a) Eq.equal * int expr * int expr
  | EMul of (int, 'a) Eq.equal * int expr * int expr
  | EIf of bool expr * 'a expr * 'a expr
  | EEq of (bool, 'a) Eq.equal * int expr * int expr
  | ELt of (bool, 'a) Eq.equal * int expr * int expr

let rec eval : 'a. 'a expr -> 'a value = fun a -> 
  match a with
  | EConstInt (x, y) -> VInt (x, y)
  | EConstBool (x, y) -> VBool (x, y)
  | EAdd (e, x, y) -> 
  	  (match (eval x, eval y) with
	  | (VInt (_, a), VInt (_, b)) -> VInt (e, (a + b))
	  | _ -> VInt (e, 0))
  | ESub (e, x, y) -> 
  	  (match (eval x, eval y) with
	  | (VInt (_, a), VInt (_, b)) -> VInt (e, (a - b))
	  | _ -> VInt (e, 0))
  | EMul (e, x, y) -> 
  	  (match (eval x, eval y) with
	  | (VInt (_, a), VInt (_, b)) -> VInt (e, (a * b))
	  | _ -> VInt (e, 0))
  | EIf (c, x, y) -> 
  	  (match eval c with
	  | VBool (_, a) -> if a then eval x else eval y
	  | _ -> eval x)
  | EEq (e, x, y) ->
  	  (match (eval x, eval y) with
	  | (VInt (_, a), VInt (_, b)) -> VBool (e, (a = b))
	  | _ -> VBool (e, false))
  | ELt (e, x, y) ->
  	  (match (eval x, eval y) with
	  | (VInt (_, a), VInt (_, b)) -> VBool (e, (a < b))
	  | _ -> VBool (e, false))

let c1 = EConstInt (Eq.refl, 1)
let c2 = EConstInt (Eq.refl, 2)
let ct = EConstBool (Eq.refl, true)
let cadd = EAdd (Eq.refl, c1, c2)
let ceq = EEq (Eq.refl, c1, c2)
let cif = EIf (ct, c1, cadd)
let err = EAdd (Eq.refl, c1, ct)
