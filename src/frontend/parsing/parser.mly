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

%type <Ast.Expr.program> program
%start program

%%
program: 
  | expr EOF { Program $1 }

expr:
  | i=INT { Number i }
  | LET; var=ID; ASSIGN; rhs=expr; IN; body=expr { Let(var, rhs, body) }
  | var=ID; {Var var}
  | left=expr; ADD; right=expr { Op(Sum(left, right)) }
  | left=expr; SUB; right=expr { Op(Sub(left, right)) }
  | TRUR { Bool true }
  | FALSE { Bool false }
  | IF; pred=expr; THEN; conseq=expr; ELSE; alt=expr { If(pred, conseq, alt) }
  | left=expr; EQUAL; right=expr { Op(Equal(left, right)) }
  | FUNC; arg=ID; RARROW; body=expr { Func(arg, body) }
  | rator=expr; rand=expr { Call(rator, rand) }
  | LCURRY; exprs=separated_list(SEMICOLON, expr); RCURRY { Sequence exprs }

