(* STATE MONAD *)
module StateMonad

let (>>=) monadBuilder f = 
  fun state -> 
    let (var,newState) = monadBuilder state
    in (f var) newState

type StateMonad() =
  member this.Bind(x,f) = x >>= f 
  member this.Return a = fun s -> (a,s)
  static member get = fun s -> (s,s)
  static member put s = fun _ -> ((),s)
  
let state = StateMonad()

type Tree<'a> =
  | Leaf of 'a
  | Node of 'a Tree * 'a Tree  

let tree =
  Node(
    Leaf 10,
    Node(
      Leaf 10,
      Node(
        Node(
          Leaf 20,
          Leaf 20),
        Node(
          Leaf 78,
          Leaf 145))))
          
let rec labelTree tree =
  state {
    match tree with
      | Leaf a ->
          let! s = StateMonad.get
          do! StateMonad.put (s+1)
          return Leaf(a,s)
      | Node (l,r) ->
          let! l' = labelTree l
          let! r' = labelTree r
          return Node(l',r')
    }

let labeledTree =  (labelTree tree) 1
