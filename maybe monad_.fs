(* MAYBE MONAD *)

module MaybeMonad

type Monad<'a> =
  | Just of 'a | Nothing

let mreturn = Just
let fail = Nothing 
let (>>=) arg f = 
  match arg with
    | Just x -> f x  
    | Nothing -> Nothing
  
(* examples *)
(* safe div *)  
let plus = fun a -> mreturn (a+a) 
let div = fun a -> if a=0 then fail else mreturn (a/a) 

let test1 = 
  mreturn 1 >>= plus >>= div
let test2 = 
  mreturn 0 >>= plus >>= div

(* lookup table *)
let phonebook = 
  [("Bob",   01788665242);
   ("Fred",  01624556442);
   ("Alice", 01889985333);
   ("Jane",  01732187565)]

let lookup (key:'a) table :Monad<'b> =
  let rec find = function
    | h::t -> if key=(fst h) then mreturn (snd h)
              else find t
    | [] -> Nothing
  in find table

let superSecretFunction  =
  fun a -> if a % 2 = 1 then Nothing
           else Just a
    
let testx1 =
  lookup "Bob" phonebook >>= superSecretFunction
let testx2 =
  lookup "asd" phonebook >>= superSecretFunction
let testx3 =
  lookup "Alice" phonebook >>= superSecretFunction
