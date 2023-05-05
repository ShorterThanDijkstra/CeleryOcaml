type expr = Let of (string * expr) list * expr | 
            Letrec of string * string list * expr * expr |
            Number of int | 
            Bool of bool |
            Op of op | 
            If of expr * expr * expr |
            Var of string |
            Func of string list * expr |
            Call of expr * expr |
            Sequence of expr list

  and op = Sum of expr * expr | 
           Sub of expr * expr | 
           Equal of expr * expr | 
           Lt of expr * expr | 
           Gt of expr * expr | 
           Debug of expr     |
           Mul of expr * expr |
           Div of expr * expr


type program = Program of expr