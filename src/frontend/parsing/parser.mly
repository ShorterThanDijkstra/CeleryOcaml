%{
    open Ast.Expr
%}

%token <int> INT
%token <string> ID
%token LET
%token EQUAL
%token ASSIGN
%token IN
%token SUB
%token ADD
%token DIV
%token MUL
%token EOF
%token TRUR 
%token FALSE 
%token IF 
%token THEN 
%token ELSE 
%token RARROW
%token FUNC
%token LCURRY
%token RCURRY
%token LPAREN
%token RPAREN
%token SEMICOLON
%token REC
%token LT 
%token GT
%token DEBUG
%token COMMA

%left ADD
%left SUB
%left MUL
%left DIV
%left GT
%left LT
%left EQUAL
%left INT
%left APP


%type <Ast.Expr.program> program
%start program

%%
program: 
  | expr EOF { Program $1 }

expr:
  | rator=expr; rand=expr; %prec APP { Call(rator, rand) }
  | LPAREN; e=expr; RPAREN { e }
  | i=INT { Number i }
  | e=let_expr { e }
  | e=letrec_expr { e }
  | var=ID; {Var var}
  | left=expr; ADD; right=expr { Op(Sum(left, right)) }
  | left=expr; SUB; right=expr { Op(Sub(left, right)) }
  | left=expr; LT; right=expr { Op(Lt(left, right)) }
  | left=expr; GT; right=expr { Op(Gt(left, right)) }
  | left=expr; MUL; right=expr { Op(Mul(left, right)) }
  | left=expr; DIV; right=expr { Op(Div(left, right)) }
  | DEBUG; e=expr { Op(Debug(e)) }
  | TRUR { Bool true }
  | FALSE { Bool false }
  | IF; pred=expr; THEN; conseq=expr; ELSE; alt=expr { If(pred, conseq, alt) }
  | left=expr; EQUAL; right=expr { Op(Equal(left, right)) }
  | FUNC; arg=ID; RARROW; body=expr { Func(arg, body) }
  | LCURRY; exprs=separated_list(SEMICOLON, expr); RCURRY { Sequence exprs }

let_expr:
  | LET; bs=separated_list(COMMA, bindings); IN; body=expr { Let(bs, body) }
  | LET; f_name=ID; arg=ID; ASSIGN; f_body=expr; IN; body=expr { Let([(f_name, Func(arg, f_body))], body) }

bindings:
  | var=ID;ASSIGN;rhs=expr { (var, rhs) }

letrec_expr: 
  | LET; REC; name=ID; arg=ID; ASSIGN; f_body=expr; IN; body=expr { Letrec(name, arg, f_body, body) }
  