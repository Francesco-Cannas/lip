%{
open Ast
%}

%token TRUE
%token FALSE
%token ZERO
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token NOT
%token AND
%token OR
%token SUCC 
%token PRED 
%token ISZERO
%token EOF

%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr: e = if_expr { e } ; 
  
if_expr: IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If(e1, e2, e3) } 
  | e = or_expr { e } ; or_expr: | e1 = or_expr; OR; e2 = and_expr { Or(e1, e2) } 
  | e = and_expr { e } ; and_expr: | e1 = and_expr; AND; e2 = not_expr { And(e1, e2) } 
  | e = not_expr { e } ; not_expr: | NOT; e = primary { Not(e) } 
  | e = primary { e } ; primary: 
    | TRUE { True } 
    | FALSE { False } 
    | ZERO { Zero } 
    | SUCC; e = primary { Succ(e) } 
    | PRED; e = primary { Pred(e) } 
    | ISZERO; e = primary { IsZero(e) }
    | LPAREN; e = expr; RPAREN { e } ;