type exprval = BoolExpVal of bool | NumberExpVal of int | ClojureExpVal of procedure
and denval = BoolDenVal of bool | NumberDenVal of int | ClojureDenVal of procedure
and procedure = Procedure of string * Ast.Expr.expr * (string, denval) Hashtbl.t


let to_exprval dv = match dv with
  | NumberDenVal i -> NumberExpVal i
  | BoolDenVal b -> BoolExpVal b
  | ClojureDenVal c -> ClojureExpVal c

let to_denval ev = match ev with
  | BoolExpVal b -> BoolDenVal b 
  | NumberExpVal i -> NumberDenVal i
  | ClojureExpVal c -> ClojureDenVal c

let show_exprval ev = match ev with
  | BoolExpVal b -> string_of_bool b 
  | NumberExpVal i -> string_of_int i
  | ClojureExpVal _ -> "<procedure>"