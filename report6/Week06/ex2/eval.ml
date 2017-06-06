open Syntax

exception Unbound

type env = (name * value) list
and value = 
    | VInt  of int
    | VBool of bool
    | VFun  of name * expr * env ref
    | VRecFun of name * name * expr * env ref

let empty_env = []
let extend x v env = (x, v) :: env

type 'a option = Some of 'a | None
let rec lookup x env =
  match env with
  | [] -> None
  | (n, v) :: xs -> if x = n then Some v else lookup x xs

exception EvalErr

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    VInt i
  | EConstBool b ->
    VBool b
  | EFun (e1, e2) -> 
     VFun (e1, e2, ref env)
  | EVar x ->
     (match lookup x env with
     | Some v -> v
     | _ -> raise Unbound)
  | EAdd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 + i2)
     | _ -> raise EvalErr)
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 - i2)
     | _ -> raise EvalErr)
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 * i2)
     | _ -> raise EvalErr)
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1, VInt i2 -> VInt (i1 / i2)
     | _ -> raise EvalErr)
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 = i2)
     | _ -> raise EvalErr)
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | VInt i1,  VInt i2  -> VBool (i1 < i2)
     | _ -> raise EvalErr)
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | VBool b ->
       if b then eval_expr env e2 else eval_expr env e3
     | _ -> raise EvalErr)
  | ELet (e1,e2,e3) ->
    let x = eval_expr env e2 in
    eval_expr (extend e1 x env) e3
  | ELetRec (f,x,e1,e2) ->
    let oenv = ref [] in
    let v = VFun(x,e1,oenv) in
      (oenv := extend f v env;
       eval_expr (extend f v env) e2)
  | EApp (e1, e2) -> 
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
      | VFun (x, e, oenv) -> eval_expr (extend x v2 !oenv) e
      | VRecFun (f, x, e, oenv) ->
              oenv := (f, (VRecFun(f,x,e,oenv))) :: !oenv;
              oenv := (x, v2) :: !oenv;
              eval_expr !oenv e
      | _ -> raise EvalErr)

let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (e1, e2) ->
          let x = eval_expr env e2 in 
          (e1, (extend e1 x env), x)
  | CRecDecl (e1, e2, e3) ->
          let oenv = ref [] in
          let x = VRecFun (e1,e2,e3,oenv) in
          oenv := extend e1 x env;
          (e1, !oenv, x)
          

let print_value x =
  match x with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun _ -> print_string "<fun>"
  | VRecFun _ -> print_string "<fun>"

