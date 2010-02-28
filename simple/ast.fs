module Ast

type Expr = 
  | IF  | THEN | ELSE
  | PLUS  | MINUS | TIMES | DIV
  | LPAREN  | RPAREN  | LBRACE  | RBRACE
  | SEMI  
  | EQUAL | ASSIGN | NOT
  | GT  | LT  | GE  | LE
  | PRINT
  | ID of string
  | INT of int
  | FLOAT of float
  | EOF
