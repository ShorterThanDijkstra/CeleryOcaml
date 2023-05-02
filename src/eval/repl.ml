open Parsing
open Eval
open Env
let parse str = 
   let lexbuf = Lexing.from_string str
   in Parser.program Lexer.read_token lexbuf

let rec repl () : unit= 
  print_string ">>> ";
  let input = read_line ()
     in try let program = parse input 
        in let res = eval_program program
           in print_endline (Mapenv.show_exprval res);
              repl ()
        with 
        | (Lexer.SyntaxError msg) -> print_endline msg; repl()
        | EvalError -> print_endline "eval error"; repl()

let () = print_endline "";
         repl ()