(* STATE MONAD *)
module StateMonad

let (>>=) monadBuilder f = 
  fun state -> 
    let (var,newState) = monadBuilder state
    in (f var) newState
let returnS a = fun s -> (a,s)
type StateMonad() =
  member this.Bind(x,f) = x >>= f 
  member this.Return a = fun s -> (a,s)
  static member get = fun s -> (s,s)
  static member put s = fun _ -> ((),s)
  
let state = StateMonad()

(* BINARY TREE *)
type Tree<'a> =
  | Leaf of 'a
  | Node of 'a Tree * 'a Tree  

let tree =
  Node(
    Leaf 10,
    Node(
      Leaf 10,
      Node(
          Leaf 20,
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

let rec labelTreee tree = 
    match tree with
      | Leaf a ->
          (fun s -> Leaf (a,s),(s+1))
      | Node(l,r) ->
          labelTreee l >>= (fun l' -> 
            labelTreee r >>= ( fun r' ->  
              (Node(l',r') |> state.Return)))
let labeledTree2 = labelTreee tree 1      

(* N ARY TREE *)
type 'a LTree = | LNil of 'a | LNode of 'a LTree list
let lt = 
  LNode [ 
    LNode [ LNil 10; LNil 24 ] ;
    LNode [ LNil 23; LNil 56; LNil 45] ; 
    LNil 35 ]

let rec lrelabelLNode f tree =
    let rec aux f tree' acc =
      state{
        match tree' with
          | x::xs -> 
              let! x' = f(x)
              return! (aux f xs (acc@[x']))
          | [] -> return acc
       } 
    in aux f tree []

let rec lrelabel tree =
  state {
    match tree with
      | LNil a->
        let! s = StateMonad.get
        do! StateMonad.put (s+1)
        return LNil(s,a)
      | LNode xs ->
          let! xs' = lrelabelLNode lrelabel xs
          return (LNode xs')
  }

let lrelabeledTree = lrelabel lt  1
