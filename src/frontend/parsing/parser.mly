%{
    open Ast.Expr
%}

%token <int> INT
%token <string> ID
%token LET
%token EQUAL
%token IN
%token SUB
%token ADD
%token EOF

%type <Ast.Expr.program> program
%start program

%%
program: 
  | expr* EOF { AProgram $1 }

expr:
  | i=INT { Const i }
  | LET; var=ID; EQUAL; rhs=expr; IN; body=expr { Let(var, rhs, body) }
  | var=ID; {Var var}
  | left=expr; ADD; right=expr { Op(Sum(left, right)) }
  | left=expr; SUB; right=expr { Op(Sub(left, right)) }


