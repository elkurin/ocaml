open Syntax
open Eval
open Infer
open TySyntax
       
let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  (try
      (let cmd = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
      let (t, newtyenv) = infer_cmd tyenv cmd in
      let (id, newenv, v) = eval_command env cmd in
      (Printf.printf "%s : " id;
       print_type t;
       print_string " = ";
       print_value v;
       print_newline ();
       read_eval_print newenv newtyenv))
   with
    | Types.TyError      
                          -> print_string "Error: Type error";
                             print_newline ();
                             read_eval_print env tyenv
    | Eval.EvalErr        -> print_string "Error: Eval error";
                             print_newline ();
                             read_eval_print env tyenv
    | Eval.MatchErr       -> print_string "Error: Matching failure.";
                             print_newline ();
                             read_eval_print env tyenv
    | Eval.Unbound msg    -> print_string "Error: Unbound ";
                             print_string msg;
                             print_newline ();
                             read_eval_print env tyenv
    | Parsing.Parse_error -> print_string "Error: Syntax error";
                             print_newline ();
                             read_eval_print env tyenv)

let _ = read_eval_print empty_env []
