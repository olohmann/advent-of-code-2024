module AdventOfCode.Days.Day07

open System.Numerics
open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Types
open Microsoft.FSharp.Core

let lines = readLines Day07 DataInput.TestData |> Array.toList

type Equation = {
    Result: bigint
    Parts: bigint list
}

// (operations: (bigint -> bigint -> bigint) list)

let solvePart1 () =
    let rec validateEquation (expectedResult: bigint) (res: bigint option) (parts: bigint list) : bool =
        match res with
        | Some value ->
            match parts with
            | [] -> expectedResult = value
            | head :: rest -> (validateEquation expectedResult (Some (value * head)) rest) || (validateEquation expectedResult (Some (value + head)) rest)
            | _ -> true
        | None -> 
            match parts with
            | [] -> failwith "Unexpected"
            | [x] -> failwith "Unexpected"
            | x :: y :: rest -> (validateEquation expectedResult (Some (x * y)) rest) || (validateEquation expectedResult (Some (x + y)) rest)
            | _ -> true

    let equations = lines |> List.map (fun l ->
        let s1 = l.Split(':')
        let s2 = s1.[1].Split(' ')
                 |> Array.map _.Trim()
                 |> Array.filter (fun x -> not (x = ""))
                 |> Array.map BigInteger.Parse
                 |> Array.toList
                 
        { Result = BigInteger.Parse s1.[0]; Parts = s2 })
    
    let x = equations |> List.filter (fun e ->
        validateEquation e.Result None e.Parts 
    )
    
    x |> List.sumBy _.Result

let solvePart2 () =
    let concatBigInteger (b1: BigInteger) (b2:BigInteger): BigInteger =
        let s1 = b1.ToString()
        let s2 = b2.ToString()
        let res = String.concat "" [s1; s2]
        BigInteger.Parse res
        
    let rec validateEquation (expectedResult: bigint) (res: bigint option) (parts: bigint list) : bool =
        match res with
        | Some value ->
            match parts with
            | [] -> expectedResult = value
            | head :: rest -> (validateEquation expectedResult (Some (value * head)) rest) || (validateEquation expectedResult (Some (value + head)) rest) || (validateEquation expectedResult (Some (concatBigInteger value head)) rest) 
            | _ -> true
        | None -> 
            match parts with
            | [] -> failwith "Unexpected"
            | [x] -> failwith "Unexpected"
            | x :: y :: rest -> (validateEquation expectedResult (Some (x * y)) rest) || (validateEquation expectedResult (Some (x + y)) rest) || (validateEquation expectedResult (Some (concatBigInteger x y)) rest) 
            | _ -> true

    let equations = lines |> List.map (fun l ->
        let s1 = l.Split(':')
        let s2 = s1.[1].Split(' ')
                 |> Array.map _.Trim()
                 |> Array.filter (fun x -> not (x = ""))
                 |> Array.map BigInteger.Parse
                 |> Array.toList
                 
        { Result = BigInteger.Parse s1.[0]; Parts = s2 })
    
    let x = equations |> List.filter (fun e ->
        validateEquation e.Result None e.Parts 
    )
    
    x |> List.sumBy _.Result
 
let run () =
    let part1Result = solvePart1()
    printfn $"Day 7, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 7, Part 2: %A{part2Result}"
