type exprval = BoolExpVal of bool | NumberExpVal of int | ClosureExpVal of procedure
and  denval = DenVal of exprval
and  procedure = Procedure of string * Ast.Expr.expr * (string, denval) Hashtbl.t


let to_exprval dv = match dv with
  | DenVal ev -> ev

let to_denval ev = DenVal ev

let show_exprval ev = match ev with
  | BoolExpVal b -> string_of_bool b 
  | NumberExpVal i -> string_of_int i
  | ClosureExpVal _ -> "<procedure>"