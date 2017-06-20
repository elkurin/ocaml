open TySyntax
exception TyError
type constraints = (ty * ty) list
type subst = (tyvar * ty) list

let rec find_subst l t =
    match l with
    | []           -> None
    | (v, a) :: xs -> if v = t then Some a else find_subst xs t
let rec ty_subst l t =
    match t with
    | TyInt      -> TyInt
    | TyBool     -> TyBool
    | TyFun(x,y) -> TyFun (ty_subst l x, ty_subst l y)
    | TyVar s  -> (match find_subst l s with
                    | None   -> t
                    | Some a -> a)

let rec compose l1 l2 =
    match l2 with
    | [] -> l1
    | (v, t) :: xs -> (v, ty_subst l1 t) :: (compose l1 xs)

let rec eq t1 t2 =
    match t1, t2 with
    | TyInt, TyInt            -> true
    | TyBool, TyBool          -> true
    | TyVar a, TyVar b        -> if a = b then true else false
    | TyFun (a,b), TyFun(c,d) -> if (eq a c) && (eq b d) then true else false
    | _, _ -> false

let rec check_unify t1 t2 =
    match t1, t2 with
    | TyFun (a,b), TyFun (c,d) -> eq t1 t2
    | TyFun (a,b), _ -> (check_unify a t2) || (check_unify b t2)
    | _, TyFun (a,b) -> (check_unify a t1) || (check_unify b t1)
    | _ -> eq t1 t2

let rec change_unify x a t =
    match x with
    | TyVar b     -> if a = b then t else x
    | TyFun (c,d) -> TyFun ((change_unify c a t), (change_unify d a t))
    | _ -> x

let rec change_list l a t =
    match l with
    | [] -> []
    | (x,y) :: xs -> ((change_unify x a t), (change_unify y a t)) :: (change_list xs a t)

let rec (ty_unify : constraints -> subst) = fun l ->
    match l with
    | [] -> []
    | (t1,t2) :: xs -> if eq t1 t2 then ty_unify xs else
                       (match t1, t2 with
                        | TyFun(s,t), TyFun(u,v) -> ty_unify ((s,u) :: (t,v) :: xs)
                        | TyVar a, _ -> if check_unify t1 t2 then raise TyError else
                            (compose (ty_unify (change_list xs a t2)) ((a,t2) :: []))
                        | _, TyVar a -> if check_unify t2 t1 then raise TyError else
                            (compose (ty_unify (change_list xs a t1)) ((a,t1) :: []))
                        | _, _       -> raise TyError)
                        
