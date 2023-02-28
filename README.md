# Little Lisp

This was an experiment to get familar with the Angstrom Parser Combinator library by creating a very small lisp.

You can run this by
- Installing ocaml and opam with your system package manager 
- `opam install angstrom dune` to do basic running
- `opam install merlin utop ocaml-lsp-server` for an editing experience. I use vscode but emacs should also work, with some configuration
- Running `dune exec ./bin/main.exe`. This will launch the REPL

You can list built-ins with the `(env)` command. Additional variable definitions are also populated to env.

