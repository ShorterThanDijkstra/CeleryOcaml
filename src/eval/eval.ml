open Ast
open Env

exception EvalError of string
let num_bin_op left right op = match (left, right) with
    | (Value.NumberExpVal i1, Value.NumberExpVal i2) -> Value.NumberExpVal (op i1 i2)
    | _ -> raise (EvalError "eval erro")

let rec eval (expr: Expr.expr) (env: (string, Value.denval) Hashtbl.t) :Value.exprval 
  = match expr with 
    | Op op -> eval_op op env
    | Let(var, rhs, body) -> 
        let rhs_val = eval rhs env
        in let new_env = Mapenv.extend_env var (Value.to_denval rhs_val) env
           in  eval body new_env                   
    | Number i -> Value.NumberExpVal i
    | Var name -> Value.to_exprval (Mapenv.apply_env name env)
    | Bool b -> BoolExpVal b
    | Sequence exprs -> eval_exprs exprs env
    | Func(arg, body) -> Value.ClosureExpVal (Value.Procedure(arg, body, env))
    | Call(rator, rand) -> let rator_val = eval rator env in 
                           let rand_val = eval rand env in 
                           apply_clojure rator_val rand_val
    | If(pred, conseq, alt) -> 
        let pred_val = eval pred env
        in match pred_val with
          | Value.BoolExpVal true -> eval conseq env
          | Value.BoolExpVal false -> eval alt env 
          | _ -> raise (EvalError "eval error")

    and eval_op op env = match op with
          | Ast.Expr.Sum(left, right) -> num_bin_op (eval left env) (eval right env) (+)
          | Ast.Expr.Sub(left, right) -> num_bin_op (eval left env) (eval right env) (-)
          | Ast.Expr.Equal(left, right) -> 
              match (eval left env, eval right env) with
                | (Value.NumberExpVal i1, Value.NumberExpVal i2) -> Value.BoolExpVal (i1 = i2)
                | _ -> raise (EvalError "eval error")
    and apply_clojure clj value = match clj with
          | Value.ClosureExpVal(Value.Procedure(arg, body, env)) -> 
              let new_env = Mapenv.extend_env arg (Value.to_denval value) env in
                  eval body new_env
          | _ -> raise (EvalError "eval error")
    and eval_exprs exprs env = match exprs with
      | []-> raise (EvalError "eval error")
      | [expr]-> eval expr env
      | (fst::rest) -> ignore (eval fst env); eval_exprs rest env

let eval_program pg = match pg with
  | Expr.Program expr -> eval expr (Mapenv.init_mapenv ())