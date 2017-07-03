open Syntax

exception Unbound of string

type env = (name * thunk) list
and thunk = Thunk of expr * env | Value of value
and value = 
    | VInt  of int
    | VBool of bool
    | VFun  of name * expr * env
    | VDFun of name * expr
    | VRecFun of name * name * expr * env
    | VPair of value * value
    | VCons of value * value
    | VNil
    | VEmpty

let rec find_match p v = 
    match p,v with
    | PInt a, VInt b  -> if a = b then Some [] else None
    | PBool a, VBool b -> if a = b then Some [] else None
    | PVar s, _ -> Some ((s, Value v) :: [])
    | PPair (a,b), VPair(c,d) | PCons (a,b), VCons(c,d) -> 
            (match find_match a c, find_match b d with
            | None, _ | _, None -> None
            | Some l1, Some l2 -> Some (l1 @ l2))
    | PNil, VNil -> Some []
    | _ -> None
 
let empty_env = []
let extend x v env = (x, v) :: env

type 'a option = Some of 'a | None
let rec lookup x env =
  match env with
  | [] -> None
  | (n, v) :: xs -> if x = n then Some v else lookup x xs

let rec fun_expr vars e =
    match vars with
    | [] -> e
    | v :: vs -> EFun (v, fun_expr vs e)

exception EvalErr
exception MatchErr

let rec eval_expr env e =
  match e with
  | EConstInt i ->
     VInt i
  | EConstBool b ->
     VBool b
  | EPair (a,b) ->
     VPair (eval_expr env a, eval_expr env b)
  | ECons (a,b) ->
     VCons (eval_expr env a, eval_expr env b)
  | ENil -> VNil
  | EFun (e1, e2) -> 
     VFun (e1, e2, env)
  | EFuns (e1, e2) -> eval_expr env (fun_expr e1 e2)
  | EDFun (e1, e2) -> 
     VDFun (e1, e2)
  | EVar x ->
     (match lookup x env with
     | Some Thunk (thunke, thunkenv) -> eval_expr thunkenv thunke
     | Some Value v -> v
     | _ -> raise (Unbound x))
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
     | VBool b1, VBool b2 -> VBool (b1 = b2)
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
    eval_expr (extend e1 (Thunk (e2, env)) env) e3
  | ELetFun (l,e1,e2) ->
    (match l with
      | [] -> raise EvalErr
      | f :: vars -> eval_expr env (ELet (f, fun_expr vars e1, e2)))
  | ERecFun (f,x,e) ->
    VRecFun (f,x,e,env)
  | ELetRec (f,x,e1,e2) ->
    let newenv = extend f (Thunk (ERecFun (f,x,e1), env)) env in eval_expr newenv e2
  | EApp (e1, e2) -> 
    let v1 = eval_expr env e1 in
    (match v1 with
      | VFun (x, e, oenv) -> eval_expr (extend x (Thunk (e2, env)) oenv) e
      | VDFun (x, e)      -> eval_expr (extend x (Thunk (e2, env)) env) e
      | VRecFun (f,x,e,oenv) ->
              let newenv = extend x (Thunk (e2, env)) (extend f (Thunk (ERecFun(f,x,e), oenv)) oenv) in
              eval_expr newenv e
      | _ -> raise EvalErr)
  | EMatch (e, l) ->
    let v = eval_expr env e in
    (match l with
    | [] -> raise MatchErr
    | (p, s) :: xs -> (match find_match p v with
                      | None -> eval_expr env (EMatch (e, xs))
                      | Some a -> eval_expr (a @ env) s))
let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (e1, e2) ->
          (("val " ^ e1), (extend e1 (Thunk (e2, env)) env), eval_expr env e2)
  | CFunDecl (l, e) ->
          (match l with
          | [] -> raise EvalErr
          | f :: vars -> eval_command env (CDecl (f, fun_expr vars e)))
  | CRecDecl (f,x,e) -> (("val " ^ f), (extend f (Thunk (ERecFun(f,x,e), env)) env), VEmpty)

let rec print_value x =
  match x with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun _  -> print_string "<fun>"
  | VDFun _ -> print_string "<fun>"
  | VRecFun _ -> print_string "<fun>"
  | VPair (a,b) -> print_string "("; print_value a; print_string ", "; print_value b; print_string ")"
  | VCons (a,b) -> print_string "["; print_value a; print_string "; "; print_value b; print_string "]"
  | VNil -> print_string "nil"
  | VEmpty  -> print_string "<fun>"
