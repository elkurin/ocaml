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
    | TyVar s    -> (match find_subst l s with
                    | None   -> t
                    | Some a -> a)
    | TyPair(x,y)-> TyPair (ty_subst l x, ty_subst l y)
    | TyList a   -> TyList (ty_subst l a)
    | TyNil      -> TyNil
    | TyExcept   -> TyExcept

let rec env_subst l env =
    match env with
    | [] -> []
    | (n, (v, t)) :: xs -> (n, (v, (ty_subst l t))) :: (env_subst l xs)

let rec compose l1 l2 =
    match l2 with
    | [] -> l1
    | (v, t) :: xs -> (v, ty_subst l1 t) :: (compose l1 xs)

let rec eq t1 t2 =
    match t1, t2 with
    | TyInt, TyInt             -> true
    | TyBool, TyBool           -> true
    | TyVar a, TyVar b         -> a = b
    | TyFun (a,b), TyFun (c,d) -> (eq a c) && (eq b d)
    | TyPair(a,b), TyPair(c,d) -> (eq a c) && (eq b d)
    | TyList a, TyList b       -> eq a b
    | TyList _, TyNil | TyNil, TyList _ -> true
    | TyNil, TyNil             -> true
    | _, _ -> false

let rec check_unify t1 t2 = 
    match t1, t2 with
    | TyFun (a,b), TyFun (c,d) -> eq t1 t2
    | TyFun (a,b), _ -> (check_unify a t2) || (check_unify b t2)
    | _, TyFun (a,b) -> (check_unify a t1) || (check_unify b t1)
    | TyPair(a,b), _ -> (check_unify a t2) || (check_unify b t2)
    | _, TyPair(a,b) -> (check_unify a t1) || (check_unify b t1)
    | _ -> eq t1 t2

    (*
let rec change_unify x a t =
    match x with
    | TyVar b     -> if a = b then t else x
    | TyFun (c,d) -> TyFun ((change_unify c a t), (change_unify d a t))
    | TyPair(c,d) -> TyPair((change_unify c a t), (change_unify d a t))
    | TyList c    -> TyList (change_unify c a t)
    | _ -> x
    *)

let rec subst_list l a t =
    match l with
    | [] -> []
    (*| (x,y) :: xs -> ((change_unify x a t), (change_unify y a t)) :: (change_list xs a t)*)
    | (x, y) :: xs  -> ((ty_subst ((a,t)::[]) x), (ty_subst ((a,t)::[]) y)) :: (subst_list xs a t)

let rec print_const l =
    match l with
    | [] -> print_string "OOOOOOOOOOOOOWARI\n"
    | (a,b) :: xs -> print_type a; print_string " $ "; print_type b; print_newline (); print_const xs

let rec (ty_unify : constraints -> subst) = fun l ->
    match l with
    | [] -> []
    | (t1,t2) :: xs -> if eq t1 t2 then ty_unify xs else
                       (match t1, t2 with
                        | TyFun(s,t), TyFun(u,v) -> ty_unify ((s,u) :: (t,v) :: xs)
                        | TyPair(s,t), TyPair(u,v) -> ty_unify ((s,u) :: (t,v) :: xs)
                        | TyList a, TyList b -> ty_unify ((a,b) :: xs)
                        | TyVar a, _ -> if check_unify t1 t2 then raise TyError else
                            (compose (ty_unify (subst_list xs a t2)) ((a,t2) :: []))
                        | _, TyVar a -> if check_unify t2 t1 then raise TyError else
                            (compose (ty_unify (subst_list xs a t1)) ((a,t1) :: []))
                        | _, _       -> raise TyError)
                        
