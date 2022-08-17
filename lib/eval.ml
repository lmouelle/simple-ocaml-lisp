open Parser

exception SyntaxError of string
exception NoSuchVariable of string

let rec sexp_to_string = function
| Number n -> string_of_int n
| Symbol s -> s
| Boolean b -> if b then "#t" else "#f"
| Primitive {name; _} -> "#" ^ name
| Quote s -> "'" ^ s
| Closure _ -> "##closure"
| List l -> 
  let rec listjoin = function
  | [] -> ""
  | head :: [] -> head
  | head :: tail -> head ^ " " ^ listjoin tail
  in
  let strings = List.map sexp_to_string l in
  "(" ^ listjoin strings ^ ")"


let prelude_environment =
  let plus = function 
  | [] -> raise @@ SyntaxError "+ 1 2 ..." 
  | args ->
    let unwrapped_nums  = List.map (function Number n -> n | _ -> raise @@ SyntaxError "+ 1 2 3") args in
    let value = List.fold_left (+) 0 unwrapped_nums in
    Number value
  in
  let minus = function
  | [Number lhs; Number rhs] ->Number (lhs - rhs)
  | _ -> raise @@ SyntaxError "- 1 2"
  in
  let multiply = function
  | [Number lhs; Number rhs] -> Number (lhs * rhs)
  | _ -> raise @@ SyntaxError "* 1 2"
  in
  let divide = function
  | [Number lhs; Number rhs] -> Number (lhs / rhs)
  | _ -> raise @@ SyntaxError "/ 1 2"
  in
  let list = function
  | [] -> raise @@ SyntaxError "list exp1 exp2"
  | args -> List args
  in
  let first = function
  | [] -> raise @@ SyntaxError "argument required"
  | head :: _ -> head
  in
  let rest = function
  | [] -> raise @@ SyntaxError "argument required"
  | _ :: tail -> List tail
  in
  let isatom = function
  | [List _] -> Boolean false
  | [_] -> Boolean true
  | _ -> raise @@ SyntaxError "(atom? term)"
  in
  let equals = function
  | [lhs; rhs] -> Boolean (lhs = rhs)
  | _ -> raise @@ SyntaxError "(= lhs rhs)"
  in
  ["+", Primitive {name = "+"; body = plus};
   "-", Primitive {name = "-"; body = minus};
   "*", Primitive {name = "*"; body = multiply};
   "/", Primitive {name = "/"; body = divide};
   "list", Primitive {name = "list"; body = list};
   "first", Primitive {name = "first"; body = first};
   "rest", Primitive {name = "rest"; body = rest};
   "atom?", Primitive {name = "atom?"; body = isatom};
   "=", Primitive {name = "="; body = equals}]

let rec eval expr env =
  let rec evalexpr = function
  | Literal (Quote q) -> Quote q
  | Literal l -> l
  | Variable name ->
    begin
      try
         List.assoc name env
      with Not_found -> raise @@ NoSuchVariable name
    end
  | If (c, t, f) ->
    begin
      match evalexpr c with
      | Boolean true -> evalexpr t
      | Boolean false -> evalexpr f
      | _ -> raise @@ SyntaxError "if bool iftrue iffalse"
    end
  |Or (c1, c2) ->
    begin
      match evalexpr c1 with
      | Boolean true -> Boolean true
      | Boolean false -> 
        begin
          match evalexpr c2 with 
          | Boolean true -> Boolean true
          | Boolean false -> Boolean false
          | _ -> raise @@ SyntaxError "or a b"
        end
      | _-> raise @@ SyntaxError "or a b"
    end
  | And (c1, c2) ->
    begin
      match evalexpr c1 with
      | Boolean false -> Boolean false
      | Boolean true ->
        begin
          match evalexpr c2 with
          | Boolean true -> Boolean true
          | Boolean false -> Boolean false
          | _ -> raise @@ SyntaxError "and a b"
        end
      | _ -> raise @@ SyntaxError "and a b"
    end
  | Call (Variable "env", []) -> 
     List (List.map (fun (name, body) -> List [Symbol name; body]) env)
  | Call (exp, args) ->
    begin
      match evalexpr exp with
      | Primitive {body; _} -> 
        let evaled_args = List.map evalexpr args in
        body evaled_args
      | Closure (params, body, closure_env) ->
        let evaled_args = List.map evalexpr args in
        let params_env = List.combine params evaled_args in
        let result, _ = eval body (params_env @ closure_env) in
        result
      | _ -> raise @@ SyntaxError "fun args*"
    end
  | Lambda (args, body) -> Closure (args, body, env)
  | Definition _ -> failwith "This should never happen"
  in
  match expr with
  | Definition (name, body) ->
    let expr_evaled, _ = eval body env in
    expr_evaled, (name, expr_evaled) :: env
  | _ -> evalexpr expr, env