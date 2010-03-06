module Program

open System.Collections.Generic
open System
open Ast

let getValue _ = 2
let addValue _ _ = ()

let rec xtree_eval e aa =
  match aa with  
  | ID_TREE a -> getValue a
  | INT_TREE a -> a
  | FLOAT_TREE a -> Convert.ToInt32(a)
  | PLUS_TREE (a,b) -> (xtree_eval e a) + (xtree_eval e b)
  | MINUS_TREE (a,b) -> (xtree_eval e a) - (xtree_eval e b)
  | TIMES_TREE (a,b) -> (xtree_eval e a ) * (xtree_eval e b)
  | DIV_TREE (a,b) -> 
        let bVal = (xtree_eval e b)
        in if bVal=0 then 0 else (xtree_eval e a)/bVal
  | EQUAL_TREE (a,b) -> 
        let aVal = (xtree_eval e a)
        let bVal = xtree_eval e b
        in if aVal = bVal then 1 else 0
  | GT_TREE (a,b) ->
        let aVal = xtree_eval e a
        let bVal = xtree_eval e b
        in if aVal > bVal then 1 else 0
  | GE_TREE (a,b) -> 
        let aVal = xtree_eval e a
        let bVal = xtree_eval e b
        in if aVal >= bVal then 1 else 0
        
let rec stmt_eval e aa =
  match aa with
   | PRINT_TREE a -> 
      let aVal = xtree_eval e a
      in () //Console.WriteLine({0}, aVal.ToString())
   | IF_TREE (a,b,c) -> 
      let test = xtree_eval e a 
      in if not ( test.Equals(0) ) then (stmt_eval e b) else (stmt_eval e c);
   | NOOP_TREE -> ()
   | ASSIGN_TREE (a,b) -> addValue a (xtree_eval e b)
   | WHILE_TREE (a,b) -> 
      let test = xtree_eval e a 
      in while ( not (test.Equals(0)) ) do stmt_eval e b done
   | STMT_GROUP_TREE a -> stmt_group_eval e a
   | _ -> ()
and stmt_group_eval e aa =
  match aa with
    | [] -> ()
    | h::t -> (stmt_eval e h); (stmt_group_eval e t);

let rec stmts_eval e = function
      [] -> ()
    | h::t -> stmt_eval e h ; stmts_eval e t;
