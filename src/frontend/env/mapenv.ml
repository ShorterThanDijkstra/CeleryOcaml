exception ApplyEnvError of string

type exprval = BoolVal of bool | NumberVal of int | ClosureVal of denval list * procedure | UnitVal
and  denval = DenVal of exprval
and  procedure = Procedure of string list * Ast.Expr.expr * env 
and  env = Empty | Extended of (string, denval) Hashtbl.t  * env | ExtendedRec of (string * string list * Ast.Expr.expr) list * env


let to_exprval dv = match dv with
  | DenVal ev -> ev

let to_denval ev = DenVal ev

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
  | ExtendedRec(bindings, saved_env) -> 
      let f_names = List.map (fun (f_name, _, _) -> f_name) bindings and
          parass = List.map (fun (_, paras, _) -> paras) bindings and
          f_bodies = List.map (fun (_, _, f_body) -> f_body) bindings  in
        (match find var f_names 0 with 
          | None -> apply_env var saved_env
          | Some ith -> let paras = List.nth parass ith and 
                            f_body = List.nth f_bodies ith in 
                        to_denval (ClosureVal([], Procedure(paras, f_body, env))))
                            
  | Extended(tb, saved_env) -> let found = Hashtbl.find_opt tb var in 
                                   match found with
                                    | None -> apply_env var saved_env
                                    | Some value -> value
  and find name names i = match names with 
                            |[] -> None
                            |(hd::rest) -> if name = hd then Some i else find name rest (i + 1)

let show_exprval ev = match ev with
  | BoolVal b -> string_of_bool b 
  | NumberVal i -> string_of_int i
  | ClosureVal _ -> "<procedure>"
  | UnitVal -> "unit"
(* 
  let rec print_env env = match env with
  | Empty -> print_endline "END\n-------------------------------"
  | ExtendedRec(bs, saved_env) -> ignore bs; print_env saved_env;
  | Extended(tb, saved_env) -> Hashtbl.iter (fun key value -> Printf.printf "%s:%s;" key (show_exprval (to_exprval value))) tb;
                               print_endline "\n";
                               print_env saved_env *)

