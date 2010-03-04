module Ast

type xtree =
  | ID_TREE of string
  | INT_TREE of System.Int32
  | FLOAT_TREE of System.Double
  | PLUS_TREE of xtree * xtree
  | MINUS_TREE of xtree * xtree
  | TIMES_TREE of xtree * xtree
  | DIV_TREE of xtree * xtree
  | EQEQ_TREE of xtree * xtree
  | GT_TREE of xtree * xtree
  | GE_TREE of xtree * xtree

type stree =
  | ASSIGN_TREE of string * xtree
  | NOOP_TREE
  | IF_TREE of xtree * stree * stree
  | WHILE_TREE of xtree * stree
  | PRINT_TREE of xtree
  | STMT_GROUP_TREE of stree list
  | PROC_TREE of string * stree
  | CALL_TREE of string
  | GET_TREE of string

  