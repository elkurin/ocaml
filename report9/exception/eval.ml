open Syntax

exception Unbound of string
exception EvalErr
exception MatchErr
exception RaiseErr of string

type env = (name * thunk ref) list
and thunk = Thunk of expr * env | Value of value
and value = 
    | VInt  of int
    | VBool of bool
    | VFun  of name * expr * env
    | VDFun of name * expr
    | VRecFun of name * name * expr * env
    | VRec  of name * thunk ref
    | VPair of thunk ref * thunk ref
    | VCons of thunk ref * thunk ref
    | VApp  of expr * expr * env
    | VExcept of name
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

let rec find_match p t = 
    match p,!t with
    | PVar s, _ -> Some ((s, t) :: [])
    | _ -> let v = (match !t with | Thunk (e,env) -> eval_expr env e | Value v -> v) in
    t := Value v;
    (match p, v with
    | _, VRec (x, thunk) -> find_match p thunk
    | _, VApp (e1, e2, oenv) -> find_match p (ref (Thunk (EAppReal(e1,e2), oenv)))
    | PInt a, VInt b  -> if a = b then Some [] else None
    | PBool a, VBool b -> if a = b then Some [] else None
    | PPair (a,b), VPair(c,d) | PCons (a,b), VCons(c,d) -> 
            (match find_match a c, find_match b d with
            | None, _ | _, None -> None
            | Some l1 , Some l2 -> Some (l1 @ l2))
    | PNil, VNil -> Some []
    | _ -> None)
and eval_expr env e =
  (*print_expr e;
  print_string "  %% eval_expr\n";*)
  match e with
  | EConstInt i ->
     VInt i
  | EConstBool b ->
     VBool b
  | EPair (a,b) ->
     VPair (ref (Thunk (a,env)), ref (Thunk (b,env)))
  | ECons (a,b) ->
     VCons (ref (Thunk (a,env)), ref (Thunk (b,env)))
  | ENil -> VNil
  | EFun (e1, e2) -> 
     VFun (e1, e2, env)
  | EFuns (e1, e2) -> eval_expr env (fun_expr e1 e2)
  | ERecFun (f,x,e) ->
    VRecFun (f,x,e,env)
  | ERec (x,e) ->
    let recr = ref (Thunk (ERec(x,e), env)) in
    let newenv = extend x recr env in
    VRec (x, ref (Thunk (e,newenv)))
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
     | VInt i1, VInt i2 -> (*print_int i1; print_string " "; print_int i2; print_newline();*) VInt (i1 + i2)
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
  | ELetRec (f,x,e1,e2) ->
    let r = ref (Thunk (ERecFun (f,x,e1), env)) in
    let newenv = extend f r env in eval_expr newenv e2
  | EApp (e1, e2) -> VApp (e1, e2, env)
  | EAppReal (e1, e2) -> 
    let v1 = eval_expr env e1 in
    (match v1 with
      | VFun (x, e, oenv) -> let r = ref (Thunk (e2, env)) in
                             eval_expr (extend x r oenv) e
      | VDFun (x, e)      -> let r = ref (Thunk (e2, env)) in
                             eval_expr (extend x r env) e
      | VRecFun (f,x,e,oenv) ->
              let r = ref (Thunk (e2, env)) in
              let recr = ref (Thunk (ERecFun(f,x,e), oenv)) in
              let newenv = extend x r (extend f recr oenv) in
              eval_expr newenv e
      | VApp (ee1,ee2,env) -> eval_expr env (EAppReal ((EAppReal (ee1, ee2)), e2))
      | _ -> print_string "EAppReal\n"; raise EvalErr)
  | EMatch (e, l) ->
    let thunk = ref (Thunk (e, env)) in
    (match l with
    | [] -> raise MatchErr
    | (p, s) :: xs -> (match find_match p thunk with
                      | None -> eval_expr env (EMatch (e, xs))
                      | Some a -> eval_expr (a @ env) s))
  | ERaise e -> raise (RaiseErr e)
(*and print_stop x =
  match x with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun _  -> print_string "VFun"
  | VDFun _ -> print_string "<fun>"
  | VRecFun _ -> print_string "VRecFun"
  | VRec (x, Value v) -> print_stop v
  | VRec (x, Thunk (e,env)) -> print_stop (eval_expr env e)
  | VPair (Thunk (a,aenv), Thunk (b,benv)) -> 
          print_string "("; print_stop (eval_expr aenv a); print_string ", "; print_stop (eval_expr benv b); print_string ")"
  | VPair (Thunk (a,aenv), Value b) -> 
          print_string "("; print_stop (eval_expr aenv a); print_string ", "; print_stop b; print_string ")"
  | VPair (Value a, Thunk (b,benv)) -> 
          print_string "("; print_stop a; print_string ", "; print_stop (eval_expr benv b); print_string ")"
  | VPair (Value a, Value b) -> 
          print_string "("; print_stop a; print_string ", "; print_stop b; print_string ")"
  | VCons (Thunk (a,aenv), Thunk (b,benv)) -> 
          print_string "["; print_expr a; print_string "; "; print_expr b; print_string "]"
  | VCons (Thunk (a, aenv), Value b) ->
          print_string "["; print_expr a; print_string "; "; print_stop b; print_string "]"
  | VCons (Value a, Thunk (b,benv)) -> 
          print_string "["; print_stop a; print_string "; "; print_expr b; print_string "]"
  | VCons (Value a, Value b) -> 
          print_string "["; print_stop a; print_string "; "; print_stop b; print_string "]"
  | VNil -> print_string "nil"
  | VApp (e1, e2, env) -> print_string "VApp "; print_expr e1; print_string " "; print_expr e2; print_newline();
  | VEmpty  -> print_string "<fun>"*)


let counter = ref 0
let rec eval_command env c =
  counter := 0;
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
  | CRecValDecl (x,e) -> let r = ref (Thunk (ERec(x,e), env)) in
                         let newenv = extend x r env in
                         (("val " ^ x), newenv, eval_expr newenv e)
  | CDeclExcept e -> let r = ref (Value (VExcept e)) in
                     ("", extend e r env, (VExcept e)) 

let rec print_value_cons x =
  counter := !counter + 1;
  if !counter > 70 then print_value x else
  match x with
  | VCons (thunka, thunkb) ->
          (match !thunka, !thunkb with
          | Thunk (a,aenv), Thunk (b,benv) ->
            print_value (eval_expr aenv a); print_string "; "; print_value_cons (eval_expr benv b)
          | Thunk (a,aenv), Value b -> 
            print_value (eval_expr aenv a); print_string "; "; print_value_cons b
          | Value a, Thunk (b,benv) -> 
            print_value a; print_string "; "; print_value_cons (eval_expr benv b)
          | Value a, Value b -> 
            print_value a; print_string "; "; print_value_cons b)
  | VNil -> print_string "]"
  | VRec (x, thunkv) -> (match !thunkv with 
                            | Value v -> print_value_cons v
                            | Thunk (e,env) -> print_value_cons (eval_expr env e))
  | VApp (e1, e2, env) -> print_value_cons (eval_expr env (EAppReal (e1, e2)))
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun (x,e,env) -> print_string x; print_string " VFUN\n"
  | VDFun _ -> print_string "VDFUN\n"
  | VRecFun _ -> print_string "VRECFUN\n"
  | VEmpty  -> print_string "VEMPTY\n"
  | VPair _ -> print_string "PAIR\n"
  | VExcept e -> print_string e
and print_value x =
  counter := !counter + 1;
  if !counter > 70 then 
      match x with
      | VRec (x, thunkv) -> (match !thunkv with 
                                | Value v -> print_value_cons v
                                | Thunk (e,env) -> print_value_cons (eval_expr env e))
      | VApp (e1, e2, env) -> print_value_cons (eval_expr env (EAppReal (e1, e2)))
      | VCons _ | VNil -> print_string ".....]"
      | VPair _ -> print_string ".....)"
      | _ -> print_string "....."
  else
  match x with
  | VInt i  -> print_int i
  | VBool b -> print_string (string_of_bool b)
  | VFun _  -> print_string "<fun>"
  | VDFun _ -> print_string "<fun>"
  | VRecFun _ -> print_string "<fun>"
  | VRec (x, thunkv) -> (match !thunkv with 
                            | Value v -> print_value v
                            | Thunk (e,env) -> print_value (eval_expr env e))
  | VPair (thunka, thunkb) ->
          (match !thunka, !thunkb with
          | Thunk (a,aenv), Thunk (b,benv) ->
            print_string "("; print_value (eval_expr aenv a); print_string ", "; print_value (eval_expr benv b); print_string ")"
          | Thunk (a,aenv), Value b -> 
            print_string "("; print_value (eval_expr aenv a); print_string ", "; print_value b; print_string ")"
          | Value a, Thunk (b,benv) -> 
            print_string "("; print_value a; print_string ", "; print_value (eval_expr benv b); print_string ")"
          | Value a, Value b -> 
            print_string "("; print_value a; print_string ", "; print_value b); print_string ")"
  | VCons _ -> print_string "["; print_value_cons x
  | VNil -> print_string "nil"
  | VApp (e1, e2, env) -> print_value (eval_expr env (EAppReal (e1, e2)))
  | VEmpty  -> print_string "<fun>"
  | VExcept e -> print_string e
