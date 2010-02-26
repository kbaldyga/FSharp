(* MAYBE MONAD *)
module MaybeMonad
let inline (>>=) m f = (^a : (member Bind: (_ -> ^b) -> ^b)(m,f))
let inline Unit x = (^b : (static member Unit : ^a -> ^b) x) 
let inline Zero () = (^a : (static member Zero : unit -> ^a) ()) 
  
type Maybe<'a> = 
  |Just of 'a 
  |Nothing
  with 
    member this.Bind f = 
      match this with
      | Just a -> f a
      | Nothing -> Nothing 
    static member Unit x = Just x
    static member Zero () = Nothing
    
(* examples *)
let plus = fun a -> Unit (a+a)
let div = fun a -> if a=0 then Zero()
                   else Unit (a/a)

let test1 = Just 0 >>= plus >>= div
let test2 = Just 10 >>= div >>= plus

(***************)
let phonebook = 
  [("Bob",   01788665242);
   ("Fred",  01624556442);
   ("Alice", 01889985333);
   ("Jane",  01732187565)]

let lookup (key:'a) table :Maybe<'b> =
  let rec find = function
    | h::t -> if key=(fst h) then Unit (snd h)
              else find t
    | [] -> Zero()
  in find table

let superSecretFunction  =
  fun a -> if a % 2 = 1 then Zero()
           else Unit a

           
let testx1 =
  lookup "Bob" phonebook >>= superSecretFunction
let testx2 =
  lookup "asd" phonebook >>= superSecretFunction
let testx3 =
  lookup "Alice" phonebook >>= superSecretFunction
