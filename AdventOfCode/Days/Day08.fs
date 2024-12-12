module AdventOfCode.Days.Day08

open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Types
open AdventOfCode.Utils.Board2D
open System.Collections.Generic

let board : Board2D<char> = parseToCharBoard Day08 DataInput.ExampleData

let createDictionaryFromBoard (board: Board2D<char>) : Dictionary<char, List<Position>> =
    let dict = Dictionary<char, List<Position>>()
    for row in 0 .. board.Length - 1 do
        for col in 0 .. board.[row].Length - 1 do
            let pos : Position = { Row = row; Col = col }
            let value = board.[row].[col]
            if value <> '.' then
                if dict.ContainsKey(value) then
                    dict.[value].Add(pos)
                else
                    dict.[value] <- List<Position>[pos]
    dict

let findAllPairwisePermutations (positions: Position list) : (Position * Position) list =
    [ for i in 0 .. positions.Length - 1 do
        for j in 0 .. positions.Length - 1 do
            if i <> j then
                yield (positions.[i], positions.[j]) ]
    
let findValidProbes (positions: Position list) =
     []

let solvePart1 () =
    let d = createDictionaryFromBoard board
    
    0

let solvePart2 () =
    0
 
let run () =
    let part1Result = solvePart1()
    printfn $"Day 8, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 8, Part 2: %A{part2Result}"
