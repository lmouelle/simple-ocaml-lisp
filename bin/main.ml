open Lib.Parser
open Lib.Eval

let rec repl env = 
  print_string "> ";
  let input = read_line () in
  match parse input with
  | Error msg -> Printf.eprintf "Error parsing string: %s" msg
  | Ok sexp ->
    try
    (
      let result, env' = eval (built_ast sexp) env in
      let result_string = sexp_to_string result in
      print_string result_string;
      print_newline ();
      repl env';
    )
    with NoSuchVariable s ->
      print_string @@ "No such variable " ^ s ^ " defined";
      print_newline ();
      repl env
    | SyntaxError s ->
      print_string @@ "Error, proper format is " ^ s;
      print_newline ();
      repl env
;;
    
let () =
  print_string "Welcome to tiny lisp. Try (env) to see a list of builtins";
  print_newline ();
  repl prelude_environment ;;   