open Eval
open Env.Mapenv
open OUnit2

let fib25 = "
let rec fib n = 
  if n < 2 
  then n 
  else fib (n - 1) + fib (n - 2) 
in fib 25
"
let even2022 = "
let rec even n = if n == 0 then true else odd (n - 1), 
        odd  n = if n == 0 then false else even (n - 1) 
in even 2022
"

let odd4088 = "
let rec even n = if n == 0 then true else odd (n - 1), 
        odd  n = if n == 0 then false else even (n - 1) 
in odd 4088
"

let add746_391 = "
let rec add x y = if y == 0 
                  then x 
                  else add x (y - 1) + 1
in add 746 391
"
let tests = "eval test" >::: [
  "fib25" >:: (fun _ -> assert_equal (NumberVal 75025) (run fib25));
  "even2022" >:: (fun _ -> assert_equal (BoolVal true) (run even2022));
  "odd4088" >:: (fun _ -> assert_equal (BoolVal false) (run odd4088));
  "add746_391" >:: (fun _ -> assert_equal (NumberVal 1137) (run add746_391));

]

let () = run_test_tt_main tests