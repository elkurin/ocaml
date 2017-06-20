open Syntax
open Types
open TySyntax

exception InferErr
type tyenv = (name * ty) list

let rec fun_expr vars e =
    match vars with
    | [] -> e
    | v :: vs -> EFun (v, fun_expr vs e)

type 'a option = Some of 'a | None

let rec lookup x env =
   match env with 
   | [] -> None
   | (n, v) :: xs -> if x = n then Some v else lookup x xs

let rec infer_expr env e =
  match e with
  | EConstInt i -> (TyInt, [])
  | EConstBool b -> (TyBool, [])
  | EFun (x, e) | EDFun (x, e) -> 
          let a = TyVar (new_tyvar ()) in 
          let (t, const) = infer_expr ((x, a) :: env) e in
          (TyFun(a, t), const)
  | EFuns (e1, e2) -> infer_expr env (fun_expr e1 e2)
  | EVar x ->
          (match lookup x env with
           | Some v -> (v, [])
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
          let (t3, const3) = infer_expr ((e1, t2) :: env) e3 in
          let const = const2 @ const3 in
          (t3, (TyVar (new_tyvar ()), t2) :: const)
  | ELetFun (l,e1,e2) ->
          (match l with
          | [] -> raise InferErr
          | f :: vars -> infer_expr env (ELet (f, fun_expr vars e1, e2)))
  | ELetRec (f,x,e1,e2) ->
          let a = TyVar (new_tyvar ()) in
          let b = TyVar (new_tyvar ()) in
          let newenv = (f, TyFun(a,b)) :: env in
          let (t1, const1) = infer_expr ((x,a) :: newenv) e1 in
          let (t2, const2) = infer_expr newenv e2 in
          let const = const1 @ const2 in
          (t2, (t1,b) :: const)
  | EApp (e1,e2) ->
          let (t1, const1) = infer_expr env e1 in
          let (t2, const2) = infer_expr env e2 in
          let const = const1 @ const2 in
          let a = TyVar (new_tyvar ()) in
          (a, (t1, TyFun(t2, a)) :: const)

let rec infer_cmd (env : tyenv) c =
  match c with
  | CExp e ->
      let (a, const) = infer_expr env e in
      let ret = ty_subst (ty_unify const) a in
      (ret, env)
  | CDecl (e1, e2) -> 
      let (a, const) = infer_expr env e2 in 
      let ret = ty_subst (ty_unify const) a in
      (ret, (e1, ret) :: env)
  | CFunDecl (l, e) -> 
      (match l with 
       | [] -> raise InferErr
       | f :: vars ->  infer_cmd env (CDecl (f, fun_expr vars e)))
  | CRecDecl (f,x,e) -> 
      let a = TyVar (new_tyvar ()) in
      let b = TyVar (new_tyvar ()) in
      let newenv = (f, TyFun(a,b)) :: env in
      let (t, const) = infer_expr ((x,a) :: newenv) e in
      let ret = ty_subst (ty_unify ((t,b) :: const)) (TyFun(a,t)) in
      (ret, (f, ret) :: env)
