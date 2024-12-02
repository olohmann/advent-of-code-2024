module AdventOfCode.Days.Day02

open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Misc
open AdventOfCode.Utils.Types

let input = inputAs2DArraySplitBySpace int Day02 DataInput.TestData

let arrPermutationsOneOff (arr: 'a[]) =
    arr
    |> Array.mapi (fun i _ ->
        Array.append (arr.[..i-1]) (arr.[i+1..])
    )

let isSafeRow (row: int[]) =
    row |> Array.mapi (fun i v ->
        if i = 0 then
              true
          elif i < row.Length - 1 then
              let left = row.[i - 1]
              let middle = row.[i]
              let right = row.[i + 1]
              let deltaLeft = abs (left - middle)
              let deltaRight = abs (right - middle)
              
              (deltaLeft > 0 && deltaLeft <= 3)
              && (deltaRight > 0 && deltaRight <= 3)
              && ((left < middle && middle < right) || (left > middle && middle > right))
          else
              true
       )
   |>  Array.fold (&&) true

let solvePart1 () =
    let res =
        input
        |> Array.map isSafeRow 
            
    let answer = res |> Array.filter id |> Array.length
    assert (answer = 534) 
    answer

let solvePart2 () =
    let res =
        input
        |> Array.map (fun arr ->
            (isSafeRow arr) || ((arrPermutationsOneOff arr) |> Array.map isSafeRow |> Array.fold (||) false)
        )
            
    let answer = res |> Array.filter id |> Array.length
    assert (answer = 577) 
    answer
 
let run () =
    let part1Result = solvePart1()
    printfn $"Day 2, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 2, Part 2: %A{part2Result}"
