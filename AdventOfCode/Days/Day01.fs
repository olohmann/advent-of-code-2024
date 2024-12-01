module AdventOfCode.Days.Day01

open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Misc
open AdventOfCode.Utils.Types

let input = inputAs2DArraySplitBySpace Day01 DataInput.TestData

let solvePart1 () =
    let left = input |> getColumn 0 |> Array.map int |> Array.sort
    let right = input |> getColumn 1 |> Array.map int|> Array.sort
    
    let res = Array.zip left right
              |> Array.map (fun (a, b) -> abs(a - b))
              |> Array.sum
    
    assert (res = 2580760)
    res

let solvePart2 () =
    let left = input |> getColumn 0 |> Array.map int
    let right = input |> getColumn 1 |> Array.map int |> Array.toSeq |> Seq.countBy id |> Map.ofSeq
    
    let res = left |> Array.map (fun (x) -> if Map.containsKey x right then right[x] * x else 0) |> Array.sum
    
    assert (res = 25358365) 
    res

let run () =
    let part1Result = solvePart1()
    printfn $"Day 1, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 1, Part 2: %A{part2Result}"