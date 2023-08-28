open Expr
type stmt = Def of (string * expr) list 

type program = Program of stmt list