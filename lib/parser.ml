open Angstrom

(*
  Sexp: Atom | ( Sexp* )
  Atom: Number | Symbol | Boolean
  Number: \d+
  Symbol: \D+
*)

type procedure = {
  name : string;
  body : sexp list -> sexp
}
and environment = (string * sexp) list
and sexp =  (* Lexer portion *)
| Number of int 
| Symbol of string 
| Boolean of bool 
| Primitive of procedure 
| Quote of string
| List of sexp list
| Closure of string list * exp * environment
and exp = (* Parser portion *)
| Literal of sexp
| Variable of string
| If of exp * exp * exp
| And of exp * exp
| Or of exp * exp
| Call of exp * exp list
| Definition of string * exp
| Lambda of string list * exp

let is_number_char = function '0' .. '9' -> true | _ -> false

let is_seperating_whitespace = function ' ' | '\t' -> true | _ -> false

let is_symbol_char = function ')' | '(' | ' ' | '\t' | ';' -> false | _ -> true

let whitespace_dropping_parser = skip_while is_seperating_whitespace

let symbol = take_while1 is_symbol_char >>= fun s -> return @@ Symbol s 

let number = take_while1 is_number_char >>= fun s -> return @@ Number (int_of_string s)

let boolean = char '#' *> (char 'f' <|> char 't') >>= function
| 't' -> return @@ Boolean true
| 'f' -> return @@ Boolean false
| _ -> fail "this should never happen"

let quote = char '\'' *> take_while1 is_symbol_char >>= fun s -> return @@ Quote s
  
let atom = number <|> boolean <|> quote <|> symbol

let sexp = fix (fun sexp ->
    let list = char '(' *> many sexp <* char ')' >>= fun l -> return @@ List l in
    whitespace_dropping_parser *> (list <|> atom) <* whitespace_dropping_parser
  )

let parse_with str parser = parse_string ~consume:All parser str

let parse s = parse_with s sexp


exception AstError of string
let rec built_ast sexp = 
  match sexp with
  | Closure _ | Primitive _ -> raise @@ AstError "Cannot build ast from function"
  | Quote _ | Number _ | Boolean _ -> Literal sexp
  | Symbol s -> Variable s
  | List [Symbol "if"; cond; iftrue; iffalse] ->
    If (built_ast cond, built_ast iftrue, built_ast iffalse)
  | List [Symbol "and"; exp1; exp2] ->
    And (built_ast exp1, built_ast exp2)
  | List [Symbol "or"; exp1; exp2] ->
    Or (built_ast exp1, built_ast exp2)
  | List [Symbol "define"; Symbol s; e] ->
    Definition (s, built_ast e)
  | List [Symbol "lambda"; List params; body] ->
    let paramnames = List.map (
      function 
      | Symbol s -> s 
      | _ -> raise @@ AstError "Cannot make closure args from non symbol") params in
    Lambda (paramnames, built_ast body)
  | List (fn :: args) ->
    Call (built_ast fn, List.map built_ast args)
  | List [] -> Literal sexp
    