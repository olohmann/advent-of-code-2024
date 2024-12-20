module AdventOfCode.Days.Day09

open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Types
open Microsoft.FSharp.Core

let diskMap = Array.head (readLines Day09 TestData)

let iterateInPairs (input: string) =
    [| for i in 0 .. 2 .. (String.length input - 1) do
        let blockId = bigint (i / 2)
        let length = int (System.Char.GetNumericValue input.[i])
        let free = if (i + 1 < String.length input - 1) then int (System.Char.GetNumericValue input.[i+1]) else 0
        
        let blockArray: bigint array = Array.create (length+free) (bigint -1)
       
        for j in  0 .. length - 1 do
            blockArray.[j] <- blockId
            
        yield blockArray
    |] |> Array.collect id

let rec compact (a: bigint array) =
    let mutable i = 0
    let mutable ordered = true
    
    while i < a.Length - 1 && ordered do
        if (i > 0) then
            let previous = a.[i-1]
            if previous = (bigint -1) && a[i] >= (bigint 0) then
                ordered <- false
        i <- i + 1
            
    if not ordered then
        let firstFreeIndex = a |> Array.findIndex (fun f -> f = (bigint -1))
        let lastNonFreeIndex = a |> Array.findIndexBack (fun f -> f >= (bigint 0))
        a.[firstFreeIndex] <- a.[lastNonFreeIndex]
        a.[lastNonFreeIndex] <- (bigint -1)
        compact(a)

let solvePart1() =
    let a = iterateInPairs diskMap
    compact a
    let res = a |> Array.filter (fun v -> v >= (bigint 0)) |> Array.mapi (fun i v -> (bigint i) * v) |> Array.sum
    res // 6370402949053 
    
let solvePart2() =
    0

let run () =
    let part1Result = solvePart1()
    printfn $"Day 9, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 9, Part 2: %A{part2Result}"
