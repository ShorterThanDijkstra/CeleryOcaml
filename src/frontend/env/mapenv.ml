exception ApplyEnvError of string

type exprval = BoolExpVal of bool | NumberExpVal of int | ClosureExpVal of procedure
and  denval = DenVal of exprval
and  procedure = Procedure of string * Ast.Expr.expr * env 
and  env = Empty | Extended of (string, denval) Hashtbl.t  * env | ExtendedRec of string * string * Ast.Expr.expr * env

let init_mapenv () :env = Empty

let extend_envs vars values env 
  = assert ((List.length vars) = (List.length values));
    let tb = Hashtbl.create (List.length vars) in 
        List.iter (fun (var, value)-> Hashtbl.add tb var value) (List.combine vars values);
        Extended(tb, env)

let rec apply_env var env = match env with
  | Empty -> raise (ApplyEnvError ("no binding for " ^ var))
  | ExtendedRec(name, arg, f_body, saved_env) -> 
      if name = var 
      then DenVal (ClosureExpVal (Procedure(arg, f_body, env)))
      else apply_env var saved_env
  | Extended(tb, saved_env) -> let find = Hashtbl.find_opt tb var in 
                                   match find with
                                    | None -> apply_env var saved_env
                                    | Some value -> value
                                    
  

let to_exprval dv = match dv with
  | DenVal ev -> ev

let to_denval ev = DenVal ev

let show_exprval ev = match ev with
  | BoolExpVal b -> string_of_bool b 
  | NumberExpVal i -> string_of_int i
  | ClosureExpVal _ -> "<procedure>"
