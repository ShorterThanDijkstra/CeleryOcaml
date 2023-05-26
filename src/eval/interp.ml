open Eval
let main () = 
  let file = Sys.argv.(1)
  in run_file file

let _ = main ()