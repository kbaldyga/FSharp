module Ast

(*
type Expr = 
  | Ident of string
  | Assign of string * Expr
  | Float of System.Double
  | Int of System.Int32
  | IfThen of Expr * Expr
  | Not of Expr
  | Times of Expr * Expr
  | Div of Expr * Expr
  | Plus of Expr * Expr
  | Minus of Expr * Expr
  | Gt of Expr * Expr
  | Ge of Expr * Expr
  | Lt of Expr * Expr
  | Le of Expr * Expr
  | Equal of Expr * Expr
  | Print of Expr
  | Skip
 *) 
  
type xtree =
  | ID_TREE of string
  | INT_TREE of int
  | PLUS_TREE of xtree * xtree
  | MINUS_TREE of xtree * xtree
  | TIMES_TREE of xtree * xtree
  | DIV_TREE of xtree * xtree
  | EQEQ_TREE of xtree * xtree
  | GT_TREE of xtree * xtree
  | GE_TREE of xtree * xtree

type stree =
  |  ASSIGN_TREE of string * xtree
  | NOOP_TREE
  | IF_TREE of xtree * stree * stree
  | WHILE_TREE of xtree * stree
  | PRINT_TREE of xtree
  | STMT_GROUP_TREE of stree list
  | PROC_TREE of string * stree
  | CALL_TREE of string
  | GET_TREE of string

  