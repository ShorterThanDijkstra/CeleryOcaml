type expr = Let of string * expr * expr | 
            Letrec of string * string * expr * expr |
            Number of int | 
            Bool of bool |
            Op of op | 
            If of expr * expr * expr |
            Var of string |
            Func of string * expr |
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