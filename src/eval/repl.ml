open Eval
open Env

let rec main () : unit= 
  print_string "λ= ";
  let input = read_line ()
     in try 
         let res = run_str input in
             print_endline (Mapenv.show_exprval res);
             main ()
        with 
          | (Parsing.Lexer.SyntaxError msg) -> print_endline msg; main()
          | EvalError -> print_endline "eval error"; main()

let () = print_endline "";
         main ()