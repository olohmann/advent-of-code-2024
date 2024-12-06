module AdventOfCode.Days.Day03

open System
open System.Text.RegularExpressions
open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Types

let input = readLines Day03 DataInput.TestData

let findAllMulTuples (input: string) =
    let pattern = @"mul\((\d+),(\d+)\)" 
    let matches = Regex.Matches(input, pattern)
    [ for m in matches -> (int m.Groups[1].Value), (int m.Groups[2].Value) ] 
    
let solvePart1 () =
    let line = String.concat "" input
    let res = line |> findAllMulTuples |> Seq.map (fun (x, y) -> x * y) |> Seq.fold (+) 0
    assert(res = 184511516)
    res

type ProcessingState =
    | Do
    | Dont
    
let findAllToken (input: string) =
    let pattern = @"mul\((\d+),(\d+)\)|do\(\)|don't\(\)" 
    let matches = Regex.Matches(input, pattern)
    matches

let rec calc (tokens: MatchCollection) (index: int) (state: ProcessingState) : int =
    if index >= tokens.Count then
        0
    else
        let token = tokens.[index].Value
        match token with
        | _ when Regex.IsMatch(token, @"mul\((\d+),(\d+)\)") ->
            if state = Do then 
                let mult = (int tokens.[index].Groups[1].Value) * (int tokens.[index].Groups[2].Value)
                mult + (calc tokens (index + 1) state)
            else
                calc tokens (index + 1) state
        | "do()" ->
            calc tokens (index + 1) Do
        | "don't()" ->
            calc tokens (index + 1) Dont
        | _ ->
            raise(Exception("Unexpected"))
            
let solvePart2 () =
   let line = String.concat "" input
   let res = calc (findAllToken line) 0 Do
   assert(res = 90044227)
   res
    
 
let run () =
    let part1Result = solvePart1()
    printfn $"Day 3, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 3, Part 2: %A{part2Result}"
