open Angstrom

(*
  Sexp: Atom | ( Sexp* )
  Atom: Number | Symbol
  Number: \d+
  Symbol: \D+
*)

type sexp = Number of int | Symbol of string | List of sexp list

let is_number_char = function '0' .. '9' -> true | _ -> false

let is_seperating_whitespace = function ' ' | '\t' -> true | _ -> false

let is_symbol_char = function ')' | '(' | ' ' | '\t' | ';' -> false | _ -> true

let whitespace_dropping_parser = skip_while is_seperating_whitespace

let symbol = take_while1 is_symbol_char >>= fun s -> return @@ Symbol s 

let number = take_while1 is_number_char >>= fun s -> return @@ Number (int_of_string s)

let atom = number <|> symbol

let sexp = fix (fun sexp ->
    let list = char '(' *> many sexp <* char ')' >>= fun l -> return @@ List l in
    whitespace_dropping_parser *> (list <|> atom) <* whitespace_dropping_parser
  )

let parse_with str parser = parse_string ~consume:All parser str

let parse s = parse_with s sexp