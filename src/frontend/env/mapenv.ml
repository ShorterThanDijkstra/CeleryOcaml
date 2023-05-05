exception ApplyEnvError of string

type exprval = BoolVal of bool | NumberVal of int | ClosureVal of denval list * procedure | UnitVal
and  denval = DenVal of exprval
and  procedure = Procedure of string list * Ast.Expr.expr * env 
and  env = Empty | Extended of (string, denval) Hashtbl.t  * env | ExtendedRec of string * string list * Ast.Expr.expr * env

let init_mapenv () :env = Empty

let extend_envs vars values env 
  = assert ((List.length vars) = (List.length values));
    let tb = Hashtbl.create (List.length vars) in 
        List.iter (fun (var, value)-> Hashtbl.add tb var value) (List.combine vars values);
        Extended(tb, env)

let extend_env var value env 
  = let tb = Hashtbl.create 1 in 
    Hashtbl.add tb var value;
    Extended(tb, env)

let rec apply_env var env = match env with
  | Empty -> raise (ApplyEnvError ("no binding for " ^ var))
  | ExtendedRec(name, args, f_body, saved_env) -> 
      if name = var 
      then DenVal (ClosureVal ([], (Procedure(args, f_body, env))))
      else apply_env var saved_env
  | Extended(tb, saved_env) -> let find = Hashtbl.find_opt tb var in 
                                   match find with
                                    | None -> apply_env var saved_env
                                    | Some value -> value

let to_exprval dv = match dv with
  | DenVal ev -> ev

let to_denval ev = DenVal ev

let show_exprval ev = match ev with
  | BoolVal b -> string_of_bool b 
  | NumberVal i -> string_of_int i
  | ClosureVal _ -> "<procedure>"
  | UnitVal -> "unit"

  let rec print_env env = match env with
  | Empty -> print_endline "END\n-------------------------------"
  | ExtendedRec(name, arg, f_body, saved_env) -> ignore(arg); ignore(f_body); print_endline ("rec: "^ name ^ "\n"); print_env saved_env;
  | Extended(tb, saved_env) -> Hashtbl.iter (fun key value -> Printf.printf "%s:%s;" key (show_exprval (to_exprval value))) tb;
                               print_endline "\n";
                               print_env saved_env

