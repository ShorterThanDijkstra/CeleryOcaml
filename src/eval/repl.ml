open Eval
open Env

let rec repl () : unit= 
  print_string "λ= ";
  let input = read_line ()
     in try 
         let res = run input in
             print_endline (Mapenv.show_exprval res);
             repl ()
        with 
          | (Parsing.Lexer.SyntaxError msg) -> print_endline msg; repl()
          | EvalError -> print_endline "eval error"; repl()

let () = print_endline "";
         repl ()