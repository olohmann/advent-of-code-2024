module AdventOfCode.Utils.Board2D

type Board2D<'T> = 'T[][]
type Position = { Row: int; Col: int }
type Kernel<'T> = {
    Left: 'T option;
    UpLeft: 'T option;
    Up: 'T option;
    UpRight: 'T option;
    Right: 'T option;
    DownRight: 'T option;
    Down: 'T option;
    DownLeft: 'T option;
    Center: 'T option;
}

type UnwrappedKernel<'T> = {
    Left: 'T
    UpLeft: 'T
    Up: 'T
    UpRight: 'T
    Right: 'T
    DownRight: 'T
    Down: 'T
    DownLeft: 'T
    Center: 'T
}

let unwrapKernel (kernel: Kernel<'T>) : UnwrappedKernel<'T> =
    {
        Left = Option.get kernel.Left
        UpLeft = Option.get kernel.UpLeft
        Up = Option.get kernel.Up
        UpRight = Option.get kernel.UpRight
        Right = Option.get kernel.Right
        DownRight = Option.get kernel.DownRight
        Down = Option.get kernel.Down
        DownLeft = Option.get kernel.DownLeft
        Center = Option.get kernel.Center
    }

let prettyPrintBoard (board: Board2D<'T>) (converter: 'T -> char) =
    board
    |> Array.map (fun a -> a |> Array.map converter)
    |> Array.iter (fun row ->
        row
        |> Array.iter (fun cell -> printf "%c" cell)
        printfn ""
    )

   
let findElement(board: Board2D<'T>) (predicate: 'T -> bool): 'T option =
    board
    |> Array.tryPick (fun row ->
        row |> Array.tryFind predicate
    )

let findElementPositions (board: Board2D<'T>) (predicate: 'T -> bool) : Position seq =
    seq {
        for rowIndex in 0 .. board.Length - 1 do
            for colIndex in 0 .. board.[rowIndex].Length - 1 do
                if predicate board.[rowIndex].[colIndex] then
                    yield { Row = rowIndex; Col = colIndex }
    }    

let findElementPosition (board: Board2D<'T>) (predicate: 'T -> bool) : Position option =
    board
    |> Array.mapi (fun rowIndex row ->
        row
        |> Array.mapi (fun colIndex cell ->
            if predicate cell then Some { Row = rowIndex; Col = colIndex } else None
        )
        |> Array.tryPick id
    )
    |> Array.tryPick id
    
let isValidPosition (board: Board2D<'T>) (pos: Position) : bool =
    pos.Row >= 0 && pos.Row < board.Length && pos.Col >= 0 && pos.Col < board.[pos.Row].Length

let createTrackingBoard (board: Board2D<'T>) : Board2D<bool> =
    Array.init board.Length (fun _ -> Array.init board.[0].Length (fun _ -> false))

let getSeq<'T> (board: Board2D<'T>) (pos: Position) (count: int) (stepRow: int) (stepCol: int) : 'T seq option =
    if isValidPosition board pos then
        let endRow = pos.Row + (count - 1) * stepRow
        let endCol = pos.Col + (count - 1) * stepCol
        if endRow >= 0 && endRow < board.Length && endCol >= 0 && endCol < board.[pos.Row].Length then
            Some (seq { for i in 0 .. count - 1 do yield board.[pos.Row + i * stepRow].[pos.Col + i * stepCol] })
        else
            None
    else
        None

let getLeftSeq<'T> (board: Board2D<'T>) (pos: Position) (count: int) : 'T seq option =
    getSeq board pos count 0 -1

let getRightSeq<'T> (board: Board2D<'T>) (pos: Position) (count: int) : 'T seq option =
    getSeq board pos count 0 1

let getUpSeq<'T> (board: Board2D<'T>) (pos: Position) (count: int) : 'T seq option =
    getSeq board pos count -1 0

let getDownSeq<'T> (board: Board2D<'T>) (pos: Position) (count: int) : 'T seq option =
    getSeq board pos count 1 0
    
let getUpLeftSeq<'T> (board: Board2D<'T>) (pos: Position) (count: int) : 'T seq option =
    getSeq board pos count -1 -1
    
let getUpRightSeq<'T> (board: Board2D<'T>) (pos: Position) (count: int) : 'T seq option =
    getSeq board pos count -1 1
    
let getDownLeftSeq<'T> (board: Board2D<'T>) (pos: Position) (count: int) : 'T seq option =
    getSeq board pos count 1 -1
    
let getDownRightSeq<'T> (board: Board2D<'T>) (pos: Position) (count: int) : 'T seq option =
    getSeq board pos count 1 1
    
let getKernel<'T> (board: Board2D<'T>) (pos: Position): Kernel<'T> =
    let getValue (rowOffset: int) (colOffset: int) =
        let newPos = { Row = pos.Row + rowOffset; Col = pos.Col + colOffset }
        if isValidPosition board newPos then Some board.[newPos.Row].[newPos.Col] else None
    {
        Left = getValue 0 -1
        UpLeft = getValue -1 -1
        Up = getValue -1 0
        UpRight = getValue -1 1
        Right = getValue 0 1
        DownRight = getValue 1 1
        Down = getValue 1 0
        DownLeft = getValue 1 -1
        Center = getValue 0 0
    }

let getAllValidDirections<'T> (board: Board2D<'T>) (pos: Position) (count: int) : 'T seq list =
    let directions = [
        getLeftSeq board pos count
        getUpLeftSeq board pos count
        getUpSeq board pos count
        getUpRightSeq board pos count
        getRightSeq board pos count
        getDownRightSeq board pos count
        getDownSeq board pos count
        getDownLeftSeq board pos count
    ]
    directions
    |> List.choose id
