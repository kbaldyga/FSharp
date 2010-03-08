// Learn more about F# at http://fsharp.net

module pm
open System
open System.Drawing
open System.Windows.Forms
open System.Windows

type Element =
    | Brick | Path    

type Board =
    Element list list
    
let PileSize = 30 
let testBoard : Board = 
    let c = Element.Brick
    let s = Element.Path
    in
    [   [ c;c;c;c;c;c;c;c;c;c;c;c ] ;
        [ c;s;s;s;s;s;s;s;s;s;s;c ] ;
        [ c;s;s;c;s;c;s;s;s;s;s;c ] ;
        [ c;s;c;s;s;c;s;s;s;s;s;c ] ;
        [ c;s;c;s;c;c;s;s;s;s;s;c ] ;
        [ c;s;s;s;s;s;s;s;s;c;s;c ] ;
        [ c;s;s;s;s;c;s;s;s;s;s;c ] ;
        [ c;s;s;s;s;c;s;s;s;s;s;c ] ;
        [ c;s;s;s;s;s;c;s;s;s;s;c ] ;
        [ c;s;s;s;s;s;s;s;s;s;s;c ] ;
        [ c;s;s;s;s;s;s;s;s;s;s;c ] ;
        [ c;c;c;c;c;c;c;c;c;c;c;c ] ;
    ]

type klocek = 
    struct
        val x : int
        val y : int
        val xsize : int
        val ysize : int
        new(x':int, y':int, xsize':int, ysize':int) = { x = x' ; y = y' ; xsize = xsize' ; ysize = ysize' }
    end

type Ludzik(xsize,ysize) =
    let mutable location = new Rectangle(30,30,xsize,ysize)
    let mutable speed = 5.0
    member self.changeLocation(x,y) = location <- new Rectangle(self.Location.X+x,self.Location.Y+y,location.Width, location.Height)
    member self.changeSpeed = fun s -> speed <- s 
    member self.Location  with get() : Rectangle = location and set(value) = location <- value
    member self.Speed with get() = speed and set(value) = speed <- value 
    
type MyForm = class
    inherit Form
    val mutable g : Graphics
    val mutable board : Board
    val mutable player : Ludzik
    val mutable label : Label
    
    override self.OnPaint e = 
        let g' = e.Graphics 
        let brush1 = Brushes.Black
        let brush2 = Brushes.Pink
        let brushP = Brushes.Aqua 
        let printLabel = 
            self.label.Text <- self.player.Location.X.ToString() + "\n" + self.player.Location.Y.ToString() + "\n"
                          + self.player.Location.Width.ToString() + "\n" + self.player.Location.Height.ToString() ;                    
        in
        List.fold ( fun acc1 row ->
            List.fold ( fun acc e  ->
                match e with 
                    | Element.Brick -> 
                        g'.FillRectangle(brush1,acc*PileSize,acc1*PileSize,PileSize,PileSize) ;
                        (acc+1)
                    | Element.Path -> 
                        g'.FillRectangle(brush2,acc*PileSize,acc1*PileSize,PileSize,PileSize) ;
                        (acc+1)
                    ) 0 row |> ignore ; // yeah, we don't want the acc (column number)
             (acc1+1) // row number ++ 
        ) 0 self.board |> ignore ;// row number also isn't interesting   
        let drawPlayer = g'.FillRectangle(brushP, self.player.Location.X, self.player.Location.Y, PileSize,PileSize) 
        in drawPlayer
         
    new () as self = { g = null ; board = [[]] ; player = new Ludzik(PileSize, PileSize) ; label=new Label()} then 
        self.SetStyle ( ControlStyles.UserPaint, true ) ;
        self.SetStyle (ControlStyles.DoubleBuffer, true ) ;
        self.SetStyle ( ControlStyles.AllPaintingInWmPaint, true );
        self.Text <- "TestApp" ;
        self.label.Height <- 100 ;
        self.label.Width <- 200 ;
        self.label.Location <- new Point(300,300) ;
        self.Controls.Add(self.label);
        self.Show() ;  
    member self.Board with get() = self.board and set(value) = self.board <- value  
    member self.Player with get() = self.player and set(value) = self.player <- value

end    
    
let form = 
    let form = new MyForm()
    form.board <- testBoard ;
    form.Width <- 500 ;
    form.Height <- 500 ;  
    form.Invalidate(); 
    form

let bricksOnBoard board = 
    let flatten list = 
        let rec aux list acc=
            match list with 
                | [] -> acc
                | h::t -> aux t (h@acc)
        in aux list []
    // that complitated funcion takes list of Elements on row, checks if any of them is Element.Brick
    // and if yes, appends its x and y coordinate to acc and return that list, so eventualy we've got
    // y [Element.Brick, Element.Path, Element.Brick] -> [(0,y),(2,y)]
    let generateRectangles list = 
        let rec aux list acc = 
            match list with 
                | [] -> List.rev acc
                | h::t -> aux t (new Rectangle(fst h * PileSize, snd h * PileSize, PileSize, PileSize)::acc)
        in aux list []
    let getBricksOnRow y rowList =
        List.fold (fun acc e ->
            let x = fst acc
            in
            match e with
                | Element.Path -> (x+1, snd acc)
                | Element.Brick -> (x+1, ((x,y)::snd acc))
            ) (0,[]) rowList 
    in
    // every row is in form of (rowNumber,[list;of;elements;on;row)
    List.fold (fun acc row -> 
        let y = fst acc in
        (y+1),(getBricksOnRow y row |> snd |> List.rev )::(snd acc) ) (0,[]) board |> snd |> flatten |> generateRectangles
    
//let player = new Ludzik(PileSize, PileSize)
type direction = Up | Down | Left | Right

let movePlayer (p:Ludzik) (dir:direction) = 
    let board = bricksOnBoard testBoard
    let howMuch = p.Speed |> Convert.ToInt32 
    let checkCollision (p:Rectangle) =
//        let x = Convert.ToInt32( p.X / PileSize )
//        let y = Convert.ToInt32( p.Y / PileSize )
//        let x' = Convert.ToInt32( (p.X+PileSize) / PileSize )
//        let y' = Convert.ToInt32( (p.Y+PileSize) / PileSize )
//        if p.X < 0 || p.Y < 0 then false else
//        if p.X+PileSize > board.Length*PileSize || p.Y+PileSize > board.Length*PileSize then false else
//        if ( board.Item(y).Item(x) = Element.Brick) then false else true
        //if ( board.Item(y').Item(x') = Element.Brick ) then false else true 
        List.fold ( fun acc e -> if p.IntersectsWith(e) then true else acc ) false board
    in
    let newLocation = 
        match dir with
            | Up -> new Rectangle(p.Location.X, p.Location.Y+howMuch,PileSize,PileSize)
            | Down -> new Rectangle(p.Location.X, p.Location.Y-howMuch,PileSize,PileSize)
            | Left -> new Rectangle(p.Location.X-howMuch, p.Location.Y,PileSize,PileSize)
            | Right -> new Rectangle(p.Location.X+howMuch, p.Location.Y,PileSize,PileSize)
    if checkCollision newLocation = false then p.Location <- newLocation ;
    p 
 
let keyStates = Array.init 300 ( fun i -> false )
   
let registerKeyEventMessage = 
    form.KeyUp.Add  ( fun e -> keyStates.[e.KeyValue] <- false);
    form.KeyDown.Add( fun e -> keyStates.[e.KeyValue] <- true ) 
 
let ticker = 
    registerKeyEventMessage
    let temp = new Timer()
    let getKeyState key = keyStates.[key]
    temp.Enabled <- true
    temp.Interval <- 10
    temp.Tick.Add(fun _ -> 
        if keyStates.[Keys.Left.GetHashCode()] = true then movePlayer (form.player) Left |> ignore
        if keyStates.[Keys.Right.GetHashCode()] = true then movePlayer (form.player) Right |> ignore
        if keyStates.[Keys.Up.GetHashCode()] = true then movePlayer (form.player) Down |> ignore
        if keyStates.[Keys.Down.GetHashCode()] = true then movePlayer (form.player) Up |> ignore
        if keyStates.[Keys.Space.GetHashCode()] = true then Application.Exit()
        form.Invalidate() ; Console.Write("x ") |> ignore )


            

do ticker
do Application.Run(form)