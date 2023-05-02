open Expr
let rec show_expr e = match e with
    | Let(var, rhs, body) -> "let " ^ var ^ " = " ^ (show_expr rhs) ^ " in " ^ (show_expr body)
    | Number i -> string_of_int i 
    | Var name -> name 
    | Op op -> show_op op
    | Bool b -> string_of_bool b
    | Func(arg, body) -> ignore arg; ignore body; "function"
    | If(pred, conseq, alt) -> "if " ^ show_expr pred ^ " then " ^ show_expr conseq ^ " else " ^ show_expr alt
    | Sequence exprs ->  List.fold_left (fun str folded -> folded ^ "\n" ^ str) "" (List.map show_expr exprs)
    | Call(rator, rand) -> show_expr rator ^ show_expr rand
    and show_op op = match op with
        | Sum (left, right) -> show_expr left ^ " + " ^ show_expr right
        | Sub (left, right) -> show_expr left ^ " - " ^ show_expr right
        | Equal (left, right) -> show_expr left ^ "==" ^ show_expr right

let show_program p = match p with
  | Program expr ->show_expr expr