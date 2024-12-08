module AdventOfCode.Days.Day06

open AdventOfCode.Utils.Board2D
open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Types

type BoardInformation =
    { mutable visited: char list
      mutable obstacle: bool
      mutable data: char }
    
type PatrolResult =
    | Loop
    | Completed
    

let headingDirection cursor: int * int =
    match cursor with
    | '^' -> -1,0
    | 'v' -> 1,0
    | '<' -> 0,-1
    | '>' -> 0,1
    | _ -> failwith "Unexpected"

let nextDirection cursor =
    match cursor with
    | '^' -> '>'
    | 'v' -> '<' 
    | '<' -> '^' 
    | '>' -> 'v' 
    | _ -> failwith "Unexpected"


let rec processBoard (board: Board2D<BoardInformation>) (pos: Position): PatrolResult =
    let currentCursor = board.[pos.Row].[pos.Col].data
    let direction: int*int = headingDirection board.[pos.Row].[pos.Col].data
    let newPosition = { Row = pos.Row + fst direction;  Col = pos.Col + snd direction }
    
    if board.[pos.Row].[pos.Col].visited |> List.contains currentCursor then
        Loop
    else
        board.[pos.Row].[pos.Col].visited <- board.[pos.Row].[pos.Col].visited @ [currentCursor]
        if not (isValidPosition board newPosition) then
            Completed
        else
            let nextBi = board.[newPosition.Row].[newPosition.Col]
            
            if nextBi.obstacle then
                board.[pos.Row].[pos.Col].data <- (nextDirection  board.[pos.Row].[pos.Col].data)
                processBoard board pos
            else
                board.[newPosition.Row].[newPosition.Col].data <- board.[pos.Row].[pos.Col].data 
                board.[pos.Row].[pos.Col].data <- '.'
                processBoard board newPosition
            
let copyBoardWithChange (board: Board2D<BoardInformation>) (pos: Position) (newValue: BoardInformation) : Board2D<'T> =
    board
    |> Array.mapi (fun rowIndex row ->
        row
        |> Array.mapi (fun colIndex cell ->
            if rowIndex = pos.Row && colIndex = pos.Col then newValue else { data = cell.data; obstacle = cell.obstacle; visited = cell.visited  }
        )
    )
 
let solvePart1 () =
    let board : Board2D<BoardInformation> = parseToCharBoard Day06 DataInput.TestData
                                            |> Array.map (fun x ->
                                                x |> Array.map (fun y ->
                                                    { visited = []; obstacle = y = '#'; data = y }
                                                )
                                            )

    let startingPosition = findElementPosition board (fun x -> x.data = '^')
    
    let boardResult = match startingPosition with
                        | Some value -> processBoard board value
                        | _ -> failwith "Unexpected: No starting pos found."
    
    //prettyPrintBoard board _.data
    
    match boardResult with
    | Loop -> failwith "Unexpected for Part 1"
    | _ -> ()
    
    let res = board |> Array.map (fun x -> x |> Array.filter (fun y -> y.visited.Length > 0)) |> Array.fold (fun acc row -> acc + row.Length) 0
    assert (res = 5080)
    res

let solvePart2 () =
    // --> would have been better to just take candidates from the path...
    let board : Board2D<BoardInformation> = parseToCharBoard Day06 DataInput.TestData
                                            |> Array.map (fun x ->
                                                x |> Array.map (fun y ->
                                                    { visited = []; obstacle = y = '#'; data = y }
                                                )
                                            )

    let startingPosition = findElementPosition board (fun x -> x.data = '^')
    
    let s = match startingPosition with
            | Some value -> value
            | None -> failwith "Unexpected"
            
    let candidatePositions = findElementPositions board (fun x -> x.data = '.') |> Seq.toArray
        
    let countLoops = candidatePositions
                        |> Array.map (fun c -> copyBoardWithChange board c { data = '#'; obstacle = true; visited = [] })
                        |> Array.map (fun b -> processBoard b s)
                        |> Array.filter _.IsLoop
                        |> Array.length
    
    assert (countLoops = 1919)
    countLoops
 
let run () =
    let part1Result = solvePart1()
    printfn $"Day 6, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 6, Part 2: %A{part2Result}"
