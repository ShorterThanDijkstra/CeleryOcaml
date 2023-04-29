open Expr
let rec show_expr e = match e with
    | Let(var, rhs, body) -> "let " ^ var ^ " = " ^ (show_expr rhs) ^ " in " ^ (show_expr body)
    | Const i -> string_of_int i 
    | Var name -> name 
    | Op op -> show_op op
    and show_op op = match op with
        | Sum (left, right) -> show_expr left ^ " + " ^ show_expr right
        | Sub (left, right) -> show_expr left ^ " - " ^ show_expr right

let show_program p = match p with
  | AProgram (exprs) -> List.fold_left (fun str folded -> folded ^ "\n" ^ str) "" (List.map show_expr exprs)