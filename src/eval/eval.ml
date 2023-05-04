open Ast.Expr
open Env.Mapenv
exception EvalError
let num_bin_op left right op = match (left, right) with
    | (NumberExpVal i1, NumberExpVal i2) -> NumberExpVal (op i1 i2)
    | _ -> raise EvalError 

let bool_bin_op left right op = match (left, right) with
| (NumberExpVal i1, NumberExpVal i2) -> BoolExpVal (op i1 i2)
| _ -> raise EvalError 

let rec eval expr env
  = 
    (* print_endline (Ast.Show.show_expr expr);  *)
    match expr with 
    | Op op -> eval_op op env
    | Let(var, rhs, body) -> 
        let rhs_val = eval rhs env
        in let new_env = extend_envs [var] [(to_denval rhs_val)] env
           in  eval body new_env 
    | Letrec(name, arg, f_body, body) -> let new_env = ExtendedRec(name, arg, f_body, env) in 
                                         eval body new_env            
    | Number i -> NumberExpVal i
    | Var name -> to_exprval (apply_env name env)
    | Bool b -> BoolExpVal b
    | Sequence exprs -> eval_exprs exprs env
    | Func(arg, body) -> ClosureExpVal (Procedure(arg, body, env))
    | Call(rator, rand) -> let rator_val = eval rator env in 
                           let rand_val = eval rand env in 
                           apply_closure rator_val rand_val
    | If(pred, conseq, alt) -> 
        let pred_val = eval pred env
        in match pred_val with
          | BoolExpVal true -> eval conseq env
          | BoolExpVal false -> eval alt env 
          | _ -> raise EvalError 
    and eval_op op env = match op with
          | Sum(left, right) -> num_bin_op (eval left env) (eval right env) (+)
          | Sub(left, right) -> num_bin_op (eval left env) (eval right env) (-)
          | Mul(left, right) -> num_bin_op (eval left env) (eval right env) ( * )
          | Div(left, right) -> num_bin_op (eval left env) (eval right env) (/)
          | Gt(left, right) -> bool_bin_op (eval left env) (eval right env) (>)
          | Lt(left, right) -> bool_bin_op (eval left env) (eval right env) (<)
          | Equal(left, right) -> bool_bin_op (eval left env) (eval right env) (=)
          | Debug(expr) -> print_endline (show_exprval (eval expr env)); Unit
          
    and apply_closure clj value = match clj with
          | ClosureExpVal(Procedure(arg, body, env)) -> 
              let new_env = extend_envs [arg] [(to_denval value)] env in
                  (* print_env new_env; *)
                  eval body new_env
          | _ -> raise EvalError 
    and eval_exprs exprs env = match exprs with
      | []-> raise EvalError
      | [expr]-> eval expr env
      | (fst::rest) -> ignore (eval fst env); eval_exprs rest env

let eval_program pg = match pg with
  | Program expr -> eval expr (init_mapenv ())