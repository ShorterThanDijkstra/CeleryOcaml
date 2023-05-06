open Expr
let rec show_expr e = match e with
    | Let(bindings, body) -> "(let " ^ "(" ^ (join_bindings bindings) ^ ")" ^ show_expr body
    | Letrec(bs, body) -> ignore bs; "(letrec " ^ show_expr body ^ ")"
    | Number i -> string_of_int i 
    | Var name -> name 
    | Op op -> show_op op
    | Bool b -> string_of_bool b
    | Func(args, body) -> ignore args; ignore body; "function"
    | If(pred, conseq, alt) -> "(if " ^ show_expr pred ^ " then " ^ show_expr conseq ^ " else " ^ show_expr alt ^")"
    | Sequence exprs ->  List.fold_left (fun str folded -> folded ^ "\n" ^ str) "" (List.map show_expr exprs)
    | Call(rator, rand) -> "(" ^ show_expr rator ^ " " ^ show_expr rand^")"
    and join_bindings bindings = match bindings with
        | [] -> ""
        | ((var, expr)::rest) -> "[" ^ var ^ " " ^ show_expr expr ^ "]" ^ join_bindings rest
    and show_op op = match op with
        | Sum (left, right) -> "(" ^ "+ " ^ show_expr left ^ " " ^show_expr right ^ ")"
        | Sub (left, right) -> "(" ^ "- " ^ show_expr left ^ " " ^show_expr right ^ ")" 
        | Mul (left, right) -> "(" ^ "* " ^ show_expr left ^ " " ^show_expr right ^ ")"
        | Div (left, right) -> "(" ^ "/ " ^ show_expr left ^ " " ^show_expr right ^ ")"
        | Equal (left, right) -> "(" ^ "== " ^ show_expr left ^ " " ^show_expr right ^ ")"
        | Lt (left, right) -> "(" ^ "< " ^ show_expr left ^ " " ^show_expr right ^ ")"
        | Gt (left, right) -> "(" ^ "> " ^ show_expr left ^ " " ^show_expr right ^ ")"
        | Debug (expr) -> "(Debug" ^ show_expr expr ^ ")"

let show_program p = match p with
  | Program expr ->show_expr expr