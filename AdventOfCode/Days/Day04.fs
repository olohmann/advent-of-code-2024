module AdventOfCode.Days.Day04

open System
open AdventOfCode.Utils.Board2D
open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Types

let board = parseToCharBoard Day04 DataInput.TestData

let solvePart1 () =
    let allPotentialSeqs = seq {
        for i in 0 .. board.Length - 1  do
            for j in 0 .. board.[i].Length - 1 do
                yield (getAllValidDirections board { Row = i; Col = j } 4 |> Seq.map (fun x -> String.Concat(x)))
    }
    
    let res = allPotentialSeqs |> Seq.concat |> Seq.filter (fun s -> s = "XMAS") |> Seq.length
    assert (res = 2545)
    res


let solvePart2 () =
    let allKernels = 
        [| for i in 0 .. board.Length - 1 do
               for j in 0 .. board.[i].Length - 1 do
                   yield getKernel board { Row = i; Col = j } |]

    let validKernels = 
        allKernels 
        |> Array.Parallel.filter (fun kernel ->
            kernel.Left.IsSome &&
            kernel.UpLeft.IsSome &&
            kernel.Up.IsSome &&
            kernel.UpRight.IsSome &&
            kernel.Right.IsSome &&
            kernel.DownRight.IsSome &&
            kernel.Down.IsSome &&
            kernel.DownLeft.IsSome &&
            kernel.Center.IsSome
        )

    let unwrappedKernels = 
        validKernels 
        |> Array.Parallel.map unwrapKernel 
        |> Array.filter (fun kernel -> kernel.Center = 'A')
        |> Array.filter (fun kernel -> ((kernel.UpLeft = 'M' && kernel.DownRight = 'S')
                                        || (kernel.UpLeft = 'S' && kernel.DownRight = 'M'))
                                       && ((kernel.UpRight = 'M' && kernel.DownLeft = 'S')
                                        || (kernel.UpRight = 'S' && kernel.DownLeft = 'M')))
        |> Array.length

    assert (unwrappedKernels = 1886)
    unwrappedKernels
 
let run () =
    let part1Result = solvePart1()
    printfn $"Day 4, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 4, Part 2: %A{part2Result}"
