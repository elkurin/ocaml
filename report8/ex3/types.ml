exception Type_Error
type tyvar = string
type ty = Int | Bool | Fun of ty * ty | Tyvar of tyvar
type subst = (tyvar * ty) list

let rec find_subst l t =
    match l with
    | []           -> None
    | (v, a) :: xs -> if v = t then Some a else find_subst xs t
let rec ty_subst l t =
    match t with
    | Int      -> Int
    | Bool     -> Bool
    | Fun(x,y) -> Fun (ty_subst l x, ty_subst l y)
    | Tyvar s  -> (match find_subst l s with
                    | None   -> t
                    | Some a -> a)

let rec compose l1 l2 =
    match l2 with
    | [] -> []
    | (v, t) :: xs -> (v, ty_subst l1 t) :: (compose l1 xs)

let rec eq t1 t2 =
    match t1, t2 with
    | Int, Int -> true
    | Bool, Bool -> true
    | Tyvar a, Tyvar b    -> if a = b then true else false
    | Fun (a,b), Fun(c,d) -> if (eq a c) && (eq b d) then true else false
    | _, _ -> false

let rec check_unify t1 t2 =
    match t1, t2 with
    | Fun (a,b), Fun (c,d) -> eq t1 t2
    | Fun (a,b), _ -> (check_unify a t2) || (check_unify b t2)
    | _ -> eq t1 t2

let rec change_unify x a t =
    match x with
    | Tyvar b    -> if a = b then t else x
    | Fun  (c,d) -> Fun ((change_unify c a t), (change_unify d a t))
    | _ -> x

let rec change_list l a t =
    match l with
    | [] -> []
    | (x,y) :: xs -> ((change_unify x a t), (change_unify y a t)) :: (change_list xs a t)

let rec (ty_unify : (ty * ty) list -> (tyvar * ty) list) = fun l ->
    match l with
    | [] -> []
    | (t1,t2) :: xs -> if eq t1 t2 then ty_unify xs else
                       (match t1, t2 with
                        | Fun(s,t), Fun(u,v) -> ty_unify ((s,u) :: (t,v) :: xs)
                        | Tyvar a, _ -> if check_unify t1 t2 then raise Type_Error else
                                        (a,t2) :: (ty_unify (change_list xs a t2))
                        | _, Tyvar a -> if check_unify t2 t1 then raise Type_Error else
                                        (a,t1) :: (ty_unify (change_list xs a t1))
                        | _, _       -> raise Type_Error)
                        
