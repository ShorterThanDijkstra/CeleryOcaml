open Ast.Expr
open Env.Mapenv
exception EvalError

let num_bin_op left right op = match (left, right) with
    | (NumberVal i1, NumberVal i2) -> NumberVal (op i1 i2)
    | _ -> raise EvalError 

let bool_bin_op left right op = match (left, right) with
| (NumberVal i1, NumberVal i2) -> BoolVal (op i1 i2)
| _ -> raise EvalError 

let rec eval expr env
  = 
    (* print_endline (Ast.Show.show_expr expr);  *)
    match expr with 
    | Op op -> eval_op op env
    | Let(bindings, body) -> 
        let vals = List.map (fun (_, rhs)-> to_denval(eval rhs env)) bindings in 
        let vars = List.map (fun (var, _) ->var) bindings in 
        let new_env = extend_envs vars vals env in 
        eval body new_env
    | Letrec(bindings, body) -> let new_env = ExtendedRec(bindings, env) in
                                    eval body new_env       
    | Number i -> NumberVal i
    | Var name -> to_exprval (apply_env name env)
    | Bool b -> BoolVal b
    | Sequence exprs -> eval_exprs exprs env
    | Func(args, body) -> ClosureVal([], (Procedure(args, body, env)))
    | Call(rator, rand) -> let rator_val = eval rator env in 
                           let rand_val = eval rand env in
                           apply_closure rator_val rand_val
    | If(pred, conseq, alt) -> 
        let pred_val = eval pred env
        in match pred_val with
          | BoolVal true -> eval conseq env
          | BoolVal false -> eval alt env 
          | _ -> raise EvalError 
    and eval_op op env = match op with
          | Sum(left, right) -> num_bin_op (eval left env) (eval right env) (+)
          | Sub(left, right) -> num_bin_op (eval left env) (eval right env) (-)
          | Mul(left, right) -> num_bin_op (eval left env) (eval right env) ( * )
          | Div(left, right) -> num_bin_op (eval left env) (eval right env) (/)
          | Gt(left, right) -> bool_bin_op (eval left env) (eval right env) (>)
          | Lt(left, right) -> bool_bin_op (eval left env) (eval right env) (<)
          | Equal(left, right) -> bool_bin_op (eval left env) (eval right env) (=)
          | Debug(expr) -> print_endline (show_exprval (eval expr env)); UnitVal
          
    and apply_closure cls value = match cls with
          | ClosureVal(args, Procedure(paras, body, env)) -> 
              let values = (List.cons (to_denval value) args) in
              if List.length values = List.length paras 
              then let new_env = extend_envs paras (List.rev values) env in 
                   eval body new_env 
              else ClosureVal(values, Procedure(paras, body, env)) (* Curry *)
          | _ -> raise EvalError 
          
    and eval_exprs exprs env = match exprs with
      | []-> raise EvalError
      | [expr]-> eval expr env
      | (fst::rest) -> ignore (eval fst env); eval_exprs rest env

let eval_program pg = match pg with
  | Program expr -> eval expr (init_mapenv ())

let parse_str str = 
  let open Parsing in
      let lexbuf = Lexing.from_string str
      in Parser.program Lexer.read_token lexbuf

let run_str str = 
     let program = parse_str str in 
     eval_program program

let parse_file file = 
  let open Parsing in 
      let lexbuf = open_in file |> Lexing.from_channel 
      in Parser.program Lexer.read_token lexbuf

let run_file file = 
  let program = parse_file file
  in eval_program program