open Parser

exception NoSuchVariable of string

type environment = (string * sexp) list

let rec sexp_to_string = function
| Number n -> string_of_int n;
| Symbol s -> s;
| Boolean b -> if b then "#t" else "#f"
| List l -> 
  let rec listjoin = function
  | [] -> ""
  | head :: [] -> head
  | head :: tail -> head ^ " " ^ listjoin tail
  in
  let strings = List.map sexp_to_string l in
  "(" ^ listjoin strings ^ ")"

let rec eval sexp (env : environment) =
  match sexp with
  | Number n -> (Number n, env)
  | Symbol s ->
    (try 
      let subsexp = List.assoc s env in
      eval subsexp env
    with Not_found -> raise @@ NoSuchVariable s)
  | Boolean b -> (Boolean b, env)
  | List (Symbol "if" :: cond :: iftrue :: iffalse :: []) -> 
      let condval, _  = eval cond env in
      let resultval = match condval with
      | Boolean true -> iftrue
      | Boolean false -> iffalse
      | _ -> failwith "(if bool exp1 exp2)"
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
  | _ ->  (sexp, env)
