open Syntax
open Eval

let rec check l =
  match l with
  | (id, Ok v) :: xs -> check xs
  | (_, Err _) :: _  -> false
  | []               -> true    

let rec print_list l =
  match l with
  | (id, Ok v) :: xs ->
      (Printf.printf "%s = " id;
       print_value v;
       print_newline ();
       print_list xs)
  | _ -> print_string ""

let rec print_err l =
  match l with
  | (_, Ok _) :: xs -> print_err xs
  | (_, Err msg) :: _ -> print_string msg
  | [] -> print_string "Err"

let rec read_eval_print env =
  print_string "# ";
  flush stdout;
  (try 
      (let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
      match eval_command env [] cmd with
      | [], newenv -> print_string "No output"
      | l, newenv ->
          (if check l then
            (print_list l;
             read_eval_print newenv)
           else
            (print_err l;
            print_newline ();
            read_eval_print env)))
  with
    | Parsing.Parse_error -> print_string "Error: Syntax error";
                             print_newline ();
                             read_eval_print env)

let initial_env =
  extend "i" (VInt 1)
	 (extend "v" (VInt 5)
		 (extend "x" (VInt 10)
			 empty_env))
    
let _ = read_eval_print initial_env
