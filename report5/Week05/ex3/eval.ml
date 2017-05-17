open Syntax

type env = (name * value) list

let empty_env = []
let extend x v env = (x, v) :: env

type 'a option = Some of 'a | None
let rec lookup x env =
  match env with
  | [] -> None
  | (n, v) :: xs -> if x = n then Some v else lookup x xs

type 'a evalErr = Ok of 'a | Err of string

let rec eval_expr env e =
  match e with
  | EConstInt i ->
    Ok (VInt i)
  | EConstBool b ->
    Ok (VBool b)
  | EVar x ->
    (match lookup x env with
     | Some v -> Ok v
     | None -> Err ("Error: Unbound value " ^ x))
  | EAdd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | Err s, _                   -> v1
     | _, Err s                   -> v2
     | Ok (VInt i1), Ok (VInt i2) -> Ok (VInt (i1 + i2))
     | Ok (VInt _),  Ok (VBool b) -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int")
     | Ok (VBool b),  _           -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int"))
  | ESub (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | Err s, _           -> v1
     | _, Err s           -> v2
     | Ok (VInt i1), Ok (VInt i2) -> Ok (VInt (i1 - i2))
     | Ok (VInt _),  Ok (VBool b) -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int")
     | Ok (VBool b),  _           -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int"))
  | EMul (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | Err s, _           -> v1
     | _, Err s           -> v2
     | Ok (VInt i1), Ok (VInt i2) -> Ok (VInt (i1 * i2))
     | Ok (VInt _),  Ok (VBool b) -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int")
     | Ok (VBool b),  _           -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int"))
  | EDiv (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | Err s, _           -> v1
     | _, Err s           -> v2
     | Ok (VInt i1), Ok (VInt i2) -> Ok (VInt (i1 / i2))
     | Ok (VInt _),  Ok (VBool b) -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int")
     | Ok (VBool b),  _           -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int"))
  | EAnd (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | Err s, _                     -> v1
     | _, Err s                     -> v2
     | Ok (VBool i1), Ok (VBool i2) -> Ok (VBool (i1 && i2))
     | Ok (VBool _),  Ok (VInt  i)  -> Err ("Error: " ^ (string_of_int i) ^ " has type int but an expression was expected of type bool")
     | Ok (VInt  i),  _             -> Err ("Error: " ^ (string_of_int i) ^ " has type int but an expression was expected of type bool"))
  | EOr (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | Err s, _           -> v1
     | _, Err s           -> v2
     | Ok (VBool i1), Ok (VBool i2) -> Ok (VBool (i1 || i2))
     | Ok (VBool _),  Ok (VInt  i)  -> Err ("Error: " ^ (string_of_int i) ^ " has type int but an expression was expected of type bool")
     | Ok (VInt  i),  _             -> Err ("Error: " ^ (string_of_int i) ^ " has type int but an expression was expected of type bool"))
  | EEq (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | Err s, _           -> v1
     | _, Err s           -> v2
     | Ok (VInt i1), Ok (VInt i2) -> Ok (VBool (i1 = i2))
     | Ok (VInt _),  Ok (VBool b) -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int")
     | Ok (VBool b),  _           -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int"))
  | ELt (e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1, v2 with
     | Err s, _           -> v1
     | _, Err s           -> v2
     | Ok (VInt i1), Ok (VInt i2) -> Ok (VBool (i1 < i2))
     | Ok (VInt _),  Ok (VBool b) -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int")
     | Ok (VBool b),  _           -> Err ("Error: " ^ (string_of_bool b) ^ " has type bool but an expression was expected of type int"))
  | EIf (e1,e2,e3) ->
    let v1 = eval_expr env e1 in
    (match v1 with
     | Ok (VBool b) ->
       if b then eval_expr env e2 else eval_expr env e3
     | Ok (VInt i) -> Err ("Error: " ^ (string_of_int i) ^ " has type int but an expression was expected of type bool"))
  | ELet (e1,e2,e3) ->
    let x = eval_expr env e2 in
    match x with
    | Err _ -> x
    | Ok y  -> eval_expr (extend e1 y env) e3
    

let rec eval_command env c =
  match c with
  | CExp e -> ("-", env, eval_expr env e)
  | CDecl (e1, e2) ->
          let x = eval_expr env e2 in 
          (match x with
          | Err _ -> (e1, env, x)
          | Ok y  -> (e1, (extend e1 y env), x))
