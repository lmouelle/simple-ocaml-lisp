open Parser

exception SyntaxError of string
exception NoSuchVariable of string

let rec sexp_to_string = function
| Number n -> string_of_int n;
| Symbol s -> s;
| Boolean b -> if b then "#t" else "#f"
| Procedure {name; _} -> "#" ^ name
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
  let list = function
  | [] -> raise @@ SyntaxError "list exp1 exp2"
  | args -> List args
  in
  ["+", Procedure {name = "+"; body = plus};
   "list", Procedure {name = "list"; body = list}]

let rec eval expr env =
  let rec evalexpr = function
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
  | Call (Variable "env", []) -> List (List.map (fun (_, b) -> b) env)
  | Call (exp, args) ->
    begin
      match evalexpr exp with
      | Procedure {body; _} -> 
        let evaled_args = List.map evalexpr args in
        body evaled_args
      | _ -> raise @@ SyntaxError "fun args*"
    end
  | Definition _ -> failwith "This should never happen"
  in
  match expr with
  | Definition (name, body) ->
    let expr_evaled, _ = eval body env in
    expr_evaled, (name, expr_evaled) :: env
  | _ -> evalexpr expr, env