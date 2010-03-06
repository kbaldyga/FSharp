module Interpreter
open System
open System.Collections.Generic

let inter input =
  let mem = new Dictionary<string,int>()

  let rec inter' input =
    match input with
        | Ident a -> mem.[a]
      //| Float a -> a
        | Int   a -> a
        | Assign (a,b) ->
            mem.[a] <- (inter' b);
        | IfThen (a,b) ->
            let boo = inter' a
            in
             if (boo :?> System.Int32) > 0 then
                (inter' b) else new obj()
        | Print a ->  Console.WriteLine("> " + a.ToString() ) ; new obj()
        | Not a -> not(inter' a) :> obj
        | Times (a,b) -> Console.WriteLine("float: " )
        | Div (a,b) -> Console.WriteLine("float: " )
        | Plus (a,b) -> Console.WriteLine("float: " )
        | Minus (a,b) -> Console.WriteLine("float: " )
        | Gt (a,b) -> Console.WriteLine("float: " )
        | Ge (a,b) -> Console.WriteLine("float: " )
        | Lt (a,b) -> Console.WriteLine("float: " )
        | Le (a,b) -> Console.WriteLine("float: " )
        | Equal (a,b) -> Console.WriteLine("float: " );
                          Console.WriteLine("{0} {1}",dic.["one"].ToString(), dic.["two"].ToString())
  in inter' input
