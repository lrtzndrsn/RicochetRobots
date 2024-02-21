module Robots
open System      

///<summary> BoardDisplay takes a row and a column and displays a board of the given size. <\summary>
///<param name>"r" - takes an integer  <\param name>
///<param name> "c" - takes an integer <\param name>
type BoardDisplay (r: int, c:int) =
    let rows = 2*r+1
    let cols = 2*c+1
    let size = (rows, cols)
    let f = Array2D.create (fst size) (snd size) ("  ")
    /// <member> Set takes a row, a column (as integers) and some content, represented as a string, and sets the contents of the string in the given row and column. <\member>
    member this.Set (row: int, col: int, cont: string) =
        f.[row, col] <- cont
    /// <member> SetBottomWall takes a row, a column and sets a BottomWall in the given row and column. <\member>
    member this.SetBottomWall (row: int, col:int) =
        f.[row,col] <- "--" 
    /// <member> SetRightWall takes a row, a column and sets a RightWall in the given row and column. <\member>
    member this.SetRightWall (row:int, col:int) = 
        f.[row,col] <- "| "
    /// <member> Show prints the board with the content. <\member>
    member this.Show () =  
        for i in 0 .. rows-1 do
            printfn ""
            for j in 0 .. cols-1 do
                if (i%2 = 0) && (j%2 = 0) then
                        this.Set (i,j, "+ ")
                printf "%s" <| f.[i,j]


type Direction = North | South | East | West
type Position = int * int
type Action =
    | Stop of Position
    | Continue of Direction * Position
    | Ignore


///<summary> The abstract class, BoardElement handles elements on the board. <\summary>
[<AbstractClass>]
type BoardElement () = 
    /// <member> RenderOn renders a Board Element on the Board. <\member>
    abstract member RenderOn : BoardDisplay -> unit
    /// <member> Interact takes a robot and a direction and makes the robot perform an action. Default action is Ignore. <\member>
    abstract member Interact : Robot -> Direction -> Action
    default __.Interact _ _ = Ignore
    /// <member> Gameover takes a Robot list and returns a bool, representing whether the game is over or not. It is false by default. <\member>
    abstract member GameOver : Robot list -> bool 
    default __.GameOver _ = false


///<summary> Robot takes a row and a column and a string. <\summary>
///<param name>"row" - takes an integer  <\param name>
///<param name> "col" - takes an integer <\param name>
///<param name> "name" - accept a string, representing the name of the robot. <\param name>
and Robot (row: int, col: int, name: string) =
    inherit BoardElement ()
    let mutable pos = (((2*row)-1),(2*col)-1)
    member this.Position
        with get () = pos
        and set (a) = pos <- a
    override this.Interact other dir =
        match dir with
        | North when (other.Position = (((fst pos)+2), snd pos)) -> Stop (other.Position)
        | South when (other.Position = (((fst pos)-2), snd pos)) -> Stop (other.Position)
        | East when (other.Position = (fst pos, ((snd pos)-2))) -> Stop (other.Position)
        | West when (other.Position = (fst pos, ((snd pos)+2))) -> Stop (other.Position)
        | _ -> Ignore
    override this.RenderOn display = 
            display.Set (fst pos, snd pos, name)
    member val Name = name with get, set
    /// Checks which direction the robot is stepping towards and updates its Position accordingly. 
    member this.Step dir =
        match dir with 
        | North -> this.Position <- ((fst pos-1), snd pos)
        | South -> this.Position <- ((fst pos+1), snd pos)
        | West -> this.Position <- (fst pos,(snd pos-1))
        | East -> this.Position <-(fst pos, (snd pos+1))

///<summary> Goal takes a row and a column reprensting the position of a Goal. <\summary>
///<param name>"row" - takes an integer  <\param name>
///<param name> "col" - takes an integer <\param name>
and Goal (row: int, col: int) = 
    inherit BoardElement ()
    let pos = (((2*row)-1), (2*col)-1)
    member this.Position = (row,col)
    override this.RenderOn display = 
            display.Set (row, col, "gg")
    override this.GameOver (r: Robot list) = not (List.forall (fun (x: Robot) -> x.Position <> this.Position) r) 

///<summary> BoardFrame takes a row and a column representing the size of the boardframe. <\summary>
///<param name>"row" - takes an integer  <\param name>
///<param name> "col" - takes an integer <\param name>
and BoardFrame (row: int, col: int) = 
    inherit BoardElement ()
    let pos = (((2*row)-1), ((2*col)-1))
    override this.Interact other dir =
        match dir with
        | North when fst other.Position = 1 -> Stop (other.Position)
        | South when fst other.Position = (fst pos) -> Stop (other.Position)
        | East when snd other.Position = (snd pos) -> Stop (other.Position)
        | West when snd other.Position = 1 -> Stop (other.Position)
        | _ -> Ignore
    override this.RenderOn display = 
        let rows = 2*row+1
        let cols = 2*col+1
        let size = (rows, cols)
        for i in 0 .. rows-1 do
                if (i%2 = 1) then
                    display.SetRightWall (i,0)
                    (display.SetRightWall (i,cols-1))
                for j in 0 .. cols-1 do
                    if (i = 0 && j%2 = 1) || (i = rows-1 && j%2 = 1) then
                        display.SetBottomWall(i,j) 


///<summary> VerticalWall takes a row and and a column and a length that specifies the position and length of a VerticalWall <\summary>
///<param name>"row" - takes an integer  <\param name>
///<param name> "col" - takes an integer <\param name>
and VerticalWall (row: int, col: int, n: int) = 
    inherit BoardElement ()
    let pos = (((2*row)-1), (2*col))
    let mutable allPos = []
    member this.Position = (row,col)
    override this.Interact other dir =
        let collisionPos = [for elem in sign n .. sign n .. ((2*n)- sign n) do if (abs (elem%2) = 1 && n > 0) then yield (((2*row)+elem-2), 2*col) elif (abs (elem%2) = 1 && n < 0) then yield (((2*row)+elem), 2*col)]
        match dir with
        | West when List.exists (fun x -> snd other.Position = snd x+1 && fst other.Position = fst x) collisionPos -> Stop (other.Position) //other.Position = (fst pos, ((snd pos)+1)) -> Stop (other.Position) 
        | East when List.exists (fun x -> snd other.Position = snd x-1 && fst other.Position = fst x) collisionPos-> Stop (other.Position)  //other.Position = (fst pos, ((snd pos)-1)) -> Stop (other.Position)
        | _ -> Ignore
    override this.RenderOn display =
        let row, col = (((2*row)-1), (2*col))
        try 
            if n > 0 then
                if n = 1 then
                    display.SetRightWall (row, col)
                else
                    display.SetRightWall (row, col)
                    for i in 1 .. 2 .. (2*(n-1)) do
                        display.SetRightWall ((row+i+1), col)
            elif n < 0 then
                if n = -1 then
                    display.SetRightWall (row, col)
                else
                    display.SetRightWall (row,col)
                    for i in -1 .. -2 .. (2*(n+1)) do
                        display.SetRightWall ((row+(i-1)), (col))
            else
                printfn "Invalid input. A wall has to have a length of at least 1."
        with 
            | :? System.IndexOutOfRangeException -> printfn "The entire wall could not fit on the board and only some of it has been drawn."

///<summary> HorizontalWall takes a row and and a column and a length that specifies the position and length of a HorizontalWall <\summary>
///<param name>"row" - takes an integer  <\param name>
///<param name> "col" - takes an integer <\param name>
and HorizontalWall (row: int, col:int, n:int) =
    inherit BoardElement ()
    let pos = ((2*row), ((2*col)-1))
    member this.Position = (row,col)
    override this.Interact other dir =
        let collisionPos = [for elem in  sign n .. sign n .. ((2*n)- sign n) do if (abs (elem%2) = 1 && n > 0) then yield (((2*row), (2*col)+elem-2)) elif (abs (elem%2) = 1 && n < 0) then yield ((2*row), ((2*col)+elem))]
        match dir with
        | South when List.exists (fun x -> fst other.Position = fst x-1 && snd other.Position = snd x) collisionPos -> Stop (other.Position)
        | North when List.exists (fun x -> fst other.Position = fst x+1 && snd other.Position = snd x) collisionPos -> Stop (other.Position)
        | _ -> Ignore
    override this.RenderOn display =
        let row,col = ((2*row),((2*col)-1))
        try 
            if n > 0 then
                if n = 1 then
                    display.SetBottomWall (row, col)
                else
                    display.SetBottomWall (row,col)
                    for i in 1 .. 2 .. (2*(n-1)) do
                    display.SetBottomWall (row, (col+i+1))
            elif n < 0 then
                if n = -1 then
                    display.SetBottomWall (row,col)
                else
                    display.SetBottomWall (row,col)
                    for i in 1 .. -2 .. (2*(n+1)) do
                    display.SetBottomWall (row, (col+i-1))
            else 
                printfn "%s" "Invalid input. A wall has to have a length of at least 1."
        with
            | :? System.IndexOutOfRangeException -> printfn "The entire wall could not fit on the board and only some of it has been drawn."

///<summary> Mirror takes a row and and a column, representing the position of which the Mirror will be on the board. When the robot enters the Mirror, 
/// its position will change. <\summary>
///<param name>"row" - takes an integer  <\param name>
///<param name> "col" - takes an integer <\param name>
and Mirror (row: int, col: int) =
    inherit BoardElement ()
    let pos = (((2*row)-1), (2*col)-1)
    member this.Position = pos
    override this.RenderOn display = 
        display.Set (fst pos, snd pos, "//")
    member this.Impact (r: Robot) = this.Position = r.Position
    override this.Interact other dir =
        if this.Impact other then
            match dir with
            | North -> Continue (East, (fst pos, ((snd pos)+1)))
            | South -> Continue (West, (fst pos, ((snd pos)-1)))
            | East -> Continue (North, (((fst pos)-1), snd pos))
            | West -> Continue (South, (((fst pos)+1), snd pos))
        else
            Ignore

///<summary> Trampoline takes a row and and a column, representing the position of which the Trampoline will be on the board. When the robot hits the trampoline
/// its Position will change. <\summary>
///<param name>"row" - takes an integer  <\param name>
///<param name> "col" - takes an integer <\param name>
and Trampoline (row: int, col:int) = 
    inherit BoardElement ()
    let pos = (((2*row)-1),(2*col)-1)
    member this.Position = pos
    override this.RenderOn display = 
        display.Set (fst pos, snd pos, "TT")
    /// <member> Checks if the robot collides with the trampoline. <\member>
    ///<param name> "r" - takes a Robot. <\param name>
    member this.Collide (r: Robot) = this.Position = r.Position
    override this.Interact other dir = 
            match dir with
            | North when (fst pos = row-2 || fst pos = row-4) && other.Position = (((fst pos)-2), snd pos) -> Stop (other.Position)
            | South when (fst pos = 1 || fst pos = row-1 || fst pos = row-3) && other.Position = (((fst pos)+2), snd pos) -> Stop (other.Position)
            | East when (snd pos = col-2 || snd pos = col-4) && other.Position = (fst pos,((snd pos)-2)) -> Stop (other.Position)
            | West when (snd pos = 1 || snd pos = 3) && other.Position = (fst pos, ((snd pos)+2)) -> Stop (other.Position)
            | _ when this.Collide other ->
                match dir with
                | North -> Continue (dir, (((fst other.Position)-4), snd other.Position))
                | South -> Continue (dir, (((fst other.Position)+4), snd other.Position))
                | East -> Continue (dir, ((fst other.Position), ((snd other.Position)+4)))
                | West -> Continue (dir, ((fst other.Position), ((snd other.Position)-4)))
            | _ -> Ignore


/// Opgave 11g2   
///<summary> Board sets up the game board, keeps track of all elements and robots on the board and can move a robot. <\summary>
type Board () =   
    let mutable elements = []
    let mutable robots = []
    /// <member> AddRobot takes a robot and adds a robot to the list of robots. <\member>
    ///<param name> "robot" - takes a robot <\param name>
    member this.AddRobot (robot: Robot) = this.Robots <- robot::robots
    /// <member> AddRobot takes an element of the type BoardElement and add its to the list of elements. <\member>
    ///<param name> "element" - takes a BoardElement <\param name>
    member this.AddElement (element: BoardElement) = this.Elements <- element::elements
    member this.Elements
        with get () = elements
        and set (a) = elements <- a 
    member this.Robots
        with get () = robots
        and set (b) = robots <- b 
    /// <member> Move takes a a robot and a direction. It moves a robot in a given direction recursively if all game elements continues to return ignore. 
    /// It stops when this is not the case <\member>
    ///<param name> "r" - takes Robot. <\param name>
    /// ///<param name> "dir" takes a Direction. <\param name>
    member this.Move (r: Robot) (dir: Direction) =
        let tempList = List.filter (fun (x:BoardElement) -> x <> (r:>BoardElement)) this.Elements
        let actionList = List.map (fun (x:BoardElement) -> x.Interact r dir) tempList
        let rec moveHelper (actions: Action List) = 
            match actions with
            | Ignore:: xs -> moveHelper xs
            | (Continue (condir, pos))::_ ->
                r.Position <- pos 
                this.Move r condir
            | (Stop pos)::_-> () 
            | [] -> 
                r.Step dir
                this.Move r dir
        moveHelper actionList     
     /// <member> RenderBoard takes a BoardDisplay and renders elements on it. <\member>
     /// <param name> "b" - takes a BoardDisplay. <\param name>
    member this.RenderBoard (b: BoardDisplay) =
            List.iter (fun (x: BoardElement) -> x.RenderOn b) this.Elements

///<summary> Game takes a Board and a Robot list. In a nutshell, it keeps track of the gameplay. <\summary>
///<param name>"b" - takes a Board.  <\param name>
///<param name> "rList" - takes a Robot list <\param name> 
type Game (b: Board, rList: Robot list) = 
    /// <member> Play makes a user able to play the game. <\member>
    member this.Play () =
        System.Console.Clear()
        printfn "Press enter to choose a new robot or use the arrow keys to control current robot."
        let mutable counter = 0
        let mutable defaultRob = rList.[0]
        let rec playHelper (b: Board) =
            let display = BoardDisplay (15,15) 
            let elements = b.Elements
            if List.exists(fun (x:BoardElement) -> x.GameOver rList) elements then
                printfn "Congratulations, you won the game!"
                printfn "Total number of moves used: %A" <| counter
            else
                let key = Console.ReadKey(true)
                match key.Key with 
                | System.ConsoleKey.Enter-> 
                    printfn "Choose a new robot by typing '1', '2', or '3': "
                    try
                        let robotKey =
                            Console.ReadLine()
                            |> Int32.Parse 
                        defaultRob <- rList.[robotKey-1]
                        playHelper b
                    with
                       _ -> printfn "That robot does not exist. Press enter to choose a new robot."; playHelper b  
                | System.ConsoleKey.UpArrow ->
                    counter <- counter + 1
                    b.Move defaultRob North
                | System.ConsoleKey.DownArrow ->
                    counter <- counter + 1
                    b.Move defaultRob South
                | System.ConsoleKey.LeftArrow ->
                    counter <- counter + 1 
                    b.Move defaultRob West
                | System.ConsoleKey.RightArrow ->
                    counter <- counter + 1 
                    b.Move defaultRob East
                | _ ->
                    printfn "Invalid input. Press an arrow key to move or press enter to choose a new robot." 
                System.Console.Clear()
                b.RenderBoard (display)
                display.Show()
                printfn "Current number of moves used: %A" <| counter
                playHelper b
        playHelper b