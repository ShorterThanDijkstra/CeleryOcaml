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
%left ID 
%left INT
%nonassoc LPAREN
%nonassoc APP /* Writing %left APP is not right. It will NOT make the rule left-associative(I don't know why) */


%type <Ast.Expr.program> program
%start program

%%
program: 
  | expr EOF { Program $1 }

expr:
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
  | FUNC; args=ID+; RARROW; body=expr { Func(args, body) }
  | LCURRY; exprs=separated_list(SEMICOLON, expr); RCURRY { Sequence exprs }
  | rator=expr; rand=expr; %prec APP { Call(rator, rand) }


let_expr:
  | LET; bs=separated_list(COMMA, let_bindings); IN; body=expr { Let(bs, body) }
  | LET; f_name=ID; args=ID*; ASSIGN; f_body=expr; IN; body=expr { Let([(f_name, Func(args, f_body))], body) }

let_bindings:
  | var=ID; ASSIGN; rhs=expr { (var, rhs) }

letrec_expr: 
  | LET; REC; bs=separated_list(COMMA, letrec_bindings); IN; body=expr { Letrec(bs, body) }

letrec_bindings:
  |f_name=ID; paras=ID+; ASSIGN; f_body=expr { (f_name, paras, f_body) }