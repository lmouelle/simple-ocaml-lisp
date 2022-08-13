open Parser

let rec eval sexp env =
  match sexp with
  | Number n -> (Number n, env)
  | Symbol s ->
    let subsexp = List.assoc s env in
    eval subsexp env
  | Boolean b -> (Boolean b, env)
  | List (Symbol "if" :: cond :: iftrue :: iffalse :: []) -> 
      let condval, _  = eval cond env in
      let resultval = match condval with
      | Boolean true -> iftrue
      | Boolean false -> iffalse
      | _ -> failwith "(if bool exp1 exp2)"
      in
      eval resultval env
  | _ ->  (sexp, env)

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
