open Syntax
open Types
open TySyntax

exception InferErr
type type_scheme = tyvar list * ty
type tyenv = (name * type_scheme) list

let rec fun_expr vars e =
    match vars with
    | [] -> e
    | v :: vs -> EFun (v, fun_expr vs e)

type 'a option = Some of 'a | None

let rec lookup x env =
   match env with 
   | [] -> None
   | (n, v) :: xs -> if x = n then Some v else lookup x xs

let rec change_type x n t =
    match t with
    | TyVar a -> if a = x then n else t
    | TyFun (a,b) -> TyFun ((change_type x n a), (change_type x n b))
    | TyPair(a,b) -> TyPair((change_type x n a), (change_type x n b))
    | TyList a    -> change_type x n a
    | _ -> t

let rec create_type l t =
    match l with
    | [] -> t
    | x :: xs -> create_type xs (change_type x (TyVar (new_tyvar ())) t)

let rec free_val l env =
    match l with
    | [] -> []
    | x :: xs -> (match lookup x env with
                 | None -> x :: (free_val xs env)
                 | Some _ -> free_val xs env)

let rec get_tyvar t =
    match t with
    | TyVar a     -> a :: []
    | TyFun (a,b) -> (get_tyvar a) @ (get_tyvar b)
    | TyPair(a,b) -> (get_tyvar a) @ (get_tyvar b)
    | TyList a    -> (get_tyvar a)
    | _ -> []

let rec match_create p =
    match p with
    | PInt _ -> (TyInt, [], [])
    | PBool _ -> (TyBool, [], [])
    | PVar x -> let a = TyVar (new_tyvar ()) in (a, [], (x, ([], a)) :: [])
    | PPair (p1, p2) -> let (t1,c1,e1) = match_create p1 in
                        let (t2,c2,e2) = match_create p2 in
                        (TyPair (t1,t2), c1 @ c2, e1 @ e2)
    | PNil -> let a = TyVar (new_tyvar ()) in (TyList a, [], [])
    | PCons (p1, p2) -> let (t1,c1,e1) = match_create p1 in
                        let (t2,c2,e2) = match_create p2 in
                        let a = TyVar (new_tyvar ()) in
                        (TyList a, (a, t1) :: (TyList a, t2) :: c1 @ c2, e1 @ e2)

let rec match_create_list l t a env = 
    match l with
    | [] -> []
    | (p, e) :: xs -> let (t1,c1,e1) = match_create p in
                      let (tt, cc) = infer_expr (e1 @ env) e in
                      let const = match_create_list xs t a env in
                      ((t, t1) :: (a, tt) :: c1 @ cc @ const)
and infer_expr env e =
  match e with
  | EConstInt i -> (TyInt, [])
  | EConstBool b -> (TyBool, [])
  | EPair (e1,e2) ->
          let (t1, const1) = infer_expr env e1 in
          let (t2, const2) = infer_expr env e2 in
          let const = const1 @ const2 in
          (TyPair (t1, t2), const)
  | ECons (e1,e2) -> 
          let (t1, const1) = infer_expr env e1 in
          let (t2, const2) = infer_expr env e2 in
          let const = const1 @ const2 in
          let a = TyVar (new_tyvar ()) in
          (TyList a, (a,t1) :: (TyList a, t2) :: const)
  | ENil -> let a = TyVar (new_tyvar ()) in 
            (TyList a, [])
  | EFun (x, e) | EDFun (x, e) -> 
          let a = TyVar (new_tyvar ()) in 
          let (t, const) = infer_expr ((x, ([], a)) :: env) e in
          (TyFun(a, t), const)
  | EFuns (e1, e2) -> infer_expr env (fun_expr e1 e2)
  | ERecFun (f,x,e) -> raise InferErr
  | ERec (x,e) -> raise InferErr
  | EVar x ->
          (match lookup x env with
           | Some (l, t) -> (create_type l t, [])
           | _ -> raise (Eval.Unbound x))
  | EAdd (e1,e2) | ESub (e1,e2) | EMul (e1,e2) | EDiv (e1,e2) ->
          let (t1, const1) = infer_expr env e1 in
          let (t2, const2) = infer_expr env e2 in
          let const = const1 @ const2 in
          (TyInt, (t1, TyInt) :: (t2, TyInt) :: const)
  | EEq (e1,e2) ->
          let (t1, const1) = infer_expr env e1 in
          let (t2, const2) = infer_expr env e2 in
          let const = const1 @ const2 in
          (TyBool, (t1, t2) :: const)
  | ELt (e1,e2) -> 
          let (t1, const1) = infer_expr env e1 in
          let (t2, const2) = infer_expr env e2 in
          let const = const1 @ const2 in
          (TyBool, (t1, TyInt) :: (t2, TyInt) :: const)
  | EIf (e1,e2,e3) ->
          let (t1, const1) = infer_expr env e1 in
          let (t2, const2) = infer_expr env e2 in
          let (t3, const3) = infer_expr env e3 in
          let const = const1 @ const2 @ const3 in
          (t2, (t1, TyBool) :: (t2, t3) :: const)
  | ELet (e1,e2,e3) ->
          let (t2, const2) = infer_expr env e2 in
          let subst = ty_unify const2 in
          let s = ty_subst subst t2 in
          let fixed = env_subst subst env in
          let p = free_val (get_tyvar s) fixed in
          let (t3, const3) = infer_expr ((e1, (p, s)) :: fixed) e3 in
          let const = const2 @ const3 in
          (t3, (TyVar (new_tyvar ()), t2) :: const)
  | ELetFun (l,e1,e2) ->
          (match l with
          | [] -> raise InferErr
          | f :: vars -> infer_expr env (ELet (f, fun_expr vars e1, e2)))
  | ELetRec (f,x,e1,e2) ->
          let a = TyVar (new_tyvar ()) in
          let b = TyVar (new_tyvar ()) in
          let newenv = (f, ([], TyFun(a,b))) :: env in
          let (t1, const1) = infer_expr ((x, ([], a)) :: newenv) e1 in
          let (t2, const2) = infer_expr newenv e2 in
          let const = const1 @ const2 in
          (t2, (t1,b) :: const)
  | EApp (e1,e2) ->
          let (t1, const1) = infer_expr env e1 in
          let (t2, const2) = infer_expr env e2 in
          let const = const1 @ const2 in
          let a = TyVar (new_tyvar ()) in
          (a, (t1, TyFun(t2, a)) :: const)
  | EMatch (e, l) ->
          let (t, const) = infer_expr env e in
          let a = TyVar (new_tyvar ()) in
          (a, (match_create_list l t a env) @ const)

let rec infer_cmd (env : tyenv) c =
  match c with
  | CExp e ->
      let (a, const) = infer_expr env e in
      let ret = ty_subst (ty_unify const) a in
      (ret, env)
  | CDecl (e1, e2) -> 
      let (a, const) = infer_expr env e2 in 
      let subst = ty_unify const in
      let s = ty_subst subst a in
      let fixed = env_subst subst env in
      let p = free_val (get_tyvar s) fixed in
      (s, (e1, (p, s)) :: fixed)
  | CFunDecl (l, e) -> 
      (match l with 
       | [] -> raise InferErr
       | f :: vars ->  infer_cmd env (CDecl (f, fun_expr vars e)))
  | CRecDecl (f,x,e) -> 
      let aa = new_tyvar () in
      let a = TyVar aa in
      let b = TyVar (new_tyvar ()) in
      let newenv = (f, ([], TyFun(a,b))) :: env in
      let (t, const) = infer_expr ((x, ([], a)) :: newenv) e in
      let subst = ty_unify ((t,b) :: const) in
      let s = ty_subst subst (TyFun (a,t)) in
      let fixed = env_subst subst env in
      let p = free_val (get_tyvar s) fixed in
      (s, (f, (p, s)) ::fixed)
  | CRecValDecl (x,e) ->
      let c = TyVar (new_tyvar ()) in
      let newenv = (x, ([], c)) :: env in
      let (a, const) = infer_expr newenv e in 
      let subst = ty_unify const in
      let s = ty_subst subst a in
      let fixed = env_subst subst env in
      let p = free_val (get_tyvar s) fixed in
      (s, (x, (p, s)) :: fixed)
