open Parser

exception SyntaxError of string
exception NoSuchVariable of string

type environment = (string * sexp) list

let rec sexp_to_string = function
| Number n -> string_of_int n;
| Symbol s -> s;
| Boolean b -> if b then "#t" else "#f"
| Procedure {name; _} -> name
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

let rec eval sexp (env : environment) =
  match sexp with
  | Number n -> Number n, env
  | Symbol s ->
    (try 
      let subsexp = List.assoc s env in
      eval subsexp env
    with Not_found -> raise @@ NoSuchVariable s)
  | Boolean b -> (Boolean b, env)
  | Procedure {name; body} -> Procedure {name; body}, env
  | List (Symbol "if" :: cond :: iftrue :: iffalse :: []) -> 
      let condval, _  = eval cond env in
      let resultval = match condval with
      | Boolean true -> iftrue
      | Boolean false -> iffalse
      | _ -> raise @@ SyntaxError "(if bool exp1 exp2)"
      in
      eval resultval env
  | List (Symbol "define" :: Symbol symbol :: expression :: []) ->
    let symbol_value, _ = eval expression env in
    let env' = (symbol, symbol_value) :: env in
    symbol_value, env'
  | List (Symbol "quote" :: expr :: []) -> Symbol (sexp_to_string expr), env
  | List (Symbol "env" :: []) ->
    let f = List.map (fun (name, value) -> List [Symbol name; value]) env in
    (List f, env)
  | List [] -> List [], env
  | List (name :: args) ->
    let evaled_fun, _ = eval name env in
    match evaled_fun with
    | Procedure {body; _} -> body args, env
    | _ -> raise @@ SyntaxError "(apply fun args)"
