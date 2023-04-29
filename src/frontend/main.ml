open Parsing
let rec repl () : unit= 
  print_string ">>> ";
  let input = read_line ()
  in let lexbuf = Lexing.from_string input
     in let program = Parser.program Lexer.read_token lexbuf
        in print_endline (Ast.Show.show_program program);
           repl ()

let () = print_endline "";
         repl ()
        
  