open Syntax

exception Unbound of string
exception EvalErr
exception MatchErr

type env = (name * thunk ref) list
and thunk = Thunk of expr * env | Value of value
and value = 
    | VInt  of int
    | VBool of bool
    | VFun  of name * expr * env
    | VDFun of name * expr
    | VRecFun of name * name * expr * env
    | VPair of thunk * thunk
    | VCons of thunk * thunk
    | VNil
    | VEmpty

let empty_env = []
let extend x v env = (x, v) :: env
let rec extend_thunk x v env = 
    match env with
    | [] -> raise EvalErr
    | (ename, ethunk) :: es -> if ename = x then ethunk := v else extend_thunk x v es

type 'a option = Some of 'a | None
let rec lookup x env =
  match env with
  | [] -> None
  | (n, v) :: xs -> if x = n then Some !v else lookup x xs

let rec fun_expr vars e =
    match vars with
    | [] -> e
    | v :: vs -> EFun (v, fun_expr vs e)

let rec find_match p v env = 
    match p,v with
    | PInt a, VInt b  -> if a = b then Some [] else None
    | PBool a, VBool b -> if a = b then Some [] else None
    | PVar s, _ -> Some ((s, ref (Value v)) :: [])
    | PPair (a,b), VPair(c,d) | PCons (a,b), VCons(c,d) -> 
           (match c, d with
            | Thunk (cexpr, cenv), Thunk (dexpr, denv) ->
                let cv = eval_expr cenv cexpr in
                let dv = eval_expr denv dexpr in
                (match find_match a cv cenv, find_match b dv denv with
                | None, _ | _, None -> None
                | Some l1, Some l2 -> Some (l1 @ l2))
            | Thunk (cexpr, cenv), Value dv ->
                let cv = eval_expr cenv cexpr in
                (match find_match a cv cenv, find_match b dv env with
                | None, _ | _, None -> None
                | Some l1, Some l2 -> Some (l1 @ l2))
            | Value cv, Thunk (dexpr, denv) ->
                let dv = eval_expr denv dexpr in
                (match find_match a cv env, find_match b dv denv with
                | None, _ | _, None -> None
                | Some l1, Some l2 -> Some (l1 @ l2))
            | Value cv, Value dv ->
                (match find_match a cv env, find_match b dv env with
                | None, _ | _, None -> None
                | Some l1, Some l2 -> Some (l1 @ l2)))
    | PNil, VNil -> Some []
    | _ -> None
and eval_expr env e =
  match e with
  | EConstInt i ->
     VInt i
  | EConstBool b ->
     VBool b
  | EPair (a,b) ->
     VPair (Thunk (a,env), Thunk (b,env))
  | ECons (a,b) ->
     VCons (Thunk (a,env), Thunk(b,env))
  | ENil -> VNil
  | EFun (e1, e2) -> 
     VFun (e1, e2, env)
  | EFuns (e1, e2) -> eval_expr env (fun_expr e1 e2)
  | EDFun (e1, e2) -> 
     VDFun (e1, e2)
  | EVar x ->
     (match lookup x env with
     | Some Thunk (thunke, thunkenv) -> let v = eval_expr thunkenv thunke in
                                        extend_thunk x (Value v) env; v
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
    let r = ref (Thunk (e2, env)) in
    eval_expr (extend e1 r env) e3
  | ELetFun (l,e1,e2) ->
    (match l with
      | [] -> raise EvalErr
      | f :: vars -> eval_expr env (ELet (f, fun_expr vars e1, e2)))
  | ERecFun (f,x,e) ->
    VRecFun (f,x,e,env)
  | ELetRec (f,x,e1,e2) ->
    let r = ref (Thunk (ERecFun (f,x,e1), env)) in
    let newenv = extend f r env in eval_expr newenv e2
  | EApp (e1, e2) -> 
    let v1 = eval_expr env e1 in
    let r = ref (Thunk (e2, env)) in
    (match v1 with
      | VFun (x, e, oenv) -> eval_expr (extend x r oenv) e
      | VDFun (x, e)      -> eval_expr (extend x r env) e
      | VRecFun (f,x,e,oenv) ->
              let recr = ref (Thunk (ERecFun(f,x,e), oenv)) in
              let newenv = extend x r (extend f recr oenv) in
              eval_expr newenv e
      | _ -> raise EvalErr)
  | EMatch (e, l) ->
    let v = eval_expr env e in
    (match l with
    | [] -> raise MatchErr
    | (p, s) :: xs -> (match find_match p v env with
                      | None -> eval_expr env (EMatch (e, xs))
                      | Some a -> eval_expr (a @ env) s))
let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (e1, e2) ->
          let r = ref (Thunk (e2, env)) in
          (("val " ^ e1), (extend e1 r env), eval_expr env e2)
  | CFunDecl (l, e) ->
          (match l with
          | [] -> raise EvalErr
          | f :: vars -> eval_command env (CDecl (f, fun_expr vars e)))
  | CRecDecl (f,x,e) -> let r = ref (Thunk (ERecFun(f,x,e), env)) in
                        (("val " ^ f), (extend f r env), VEmpty)

let counter = ref 0
let rec print_value x =
  counter := !counter + 1;
  if !counter > 1000 then () else
  (match x with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun _  -> print_string "<fun>"
  | VDFun _ -> print_string "<fun>"
  | VRecFun _ -> print_string "<fun>"
  | VPair (Thunk (a,aenv), Thunk (b,benv)) -> 
          print_string "("; print_value (eval_expr aenv a); print_string ", "; print_value (eval_expr benv b); print_string ")"
  | VPair (Thunk (a,aenv), Value b) -> 
          print_string "("; print_value (eval_expr aenv a); print_string ", "; print_value b; print_string ")"
  | VPair (Value a, Thunk (b,benv)) -> 
          print_string "("; print_value a; print_string ", "; print_value (eval_expr benv b); print_string ")"
  | VPair (Value a, Value b) -> 
          print_string "("; print_value a; print_string ", "; print_value b; print_string ")"
  | VCons (Thunk (a,aenv), Thunk (b,benv)) -> 
          print_string "["; print_value (eval_expr aenv a); print_string "; "; print_value (eval_expr benv b); print_string "]"
  | VCons (Thunk (a,aenv), Value b) -> 
          print_string "["; print_value (eval_expr aenv a); print_string "; "; print_value b; print_string "]"
  | VCons (Value a, Thunk (b,benv)) -> 
          print_string "["; print_value a; print_string "; "; print_value (eval_expr benv b); print_string "]"
  | VCons (Value a, Value b) -> 
          print_string "["; print_value a; print_string "; "; print_value b; print_string "]"
  | VNil -> print_string "nil"
  | VEmpty  -> print_string "<fun>")
