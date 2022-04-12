open Angstrom

(*
  Sexp: Atom | ( Sexp* )
  Atom: Number | Symbol
  Number: \d+
  Symbol: \D+
*)

let is_number_char = function '0' .. '9' -> true | _ -> false

let symbol = take_while1 (fun c -> is_number_char c |> not && c <> ')' && c <> '(') >>= fun s -> return @@ `Symbol s

let number = take_while1 is_number_char >>= fun s -> return @@ `Number (int_of_string s)

let atom = symbol <|> number

let sexp = fix (fun sexp ->
    let list = char '(' *> many sexp <* char ')' >>= fun l -> return @@ `List l in
    list <|> atom
  )

let parse_with str parser = parse_string ~consume:All parser str

let parse s = parse_with s sexp