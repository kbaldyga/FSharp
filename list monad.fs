(* LIST MONAD *)
module ListMonad

type List<'a> = List of ('a -> 'a list)

type ListMonad() =
  member this.Return((a:'a)) = [a]
  member this.Bind((xs:'b list), f) =
      List.concat (List.map (fun x -> f x) xs)
  member this.Zero() = []
 
let generateSomething z =
  [z+6;z+7]
  
let list = ListMonad()

let z = 
  list {
    let! a = [1..3]
    let! b = generateSomething a
    return (a,b)
  }
  
let zz =
  list {
    let! a = [1..20]
    if a%2=0 then return a 
    else list.Zero()
   }
