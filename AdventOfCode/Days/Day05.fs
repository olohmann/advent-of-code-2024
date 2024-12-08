module AdventOfCode.Days.Day05

open AdventOfCode.Utils.TopologicalSort
open System.Collections.Generic
open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Types

let lines = readLines Day05 DataInput.TestData |> Array.toList

type Page = int
type PageOrderingRule = Page*Page 
type PageOrderingRules = PageOrderingRule list 
type Position = int
type PageUpdates = int list list
type PageUpdate = int list
type PagePositions = IDictionary<Position, Page>

let isCorrectPageUpdate (pageOrderingRules: PageOrderingRules) (pageUpdate: PageUpdate) : bool =
    let pagePositions : PagePositions = pageUpdate |> List.mapi (fun i p -> p, i) |> dict
      
    pageOrderingRules |> List.forall (fun (x,y) ->
        if pagePositions.ContainsKey(x) && pagePositions.ContainsKey(y) then
            pagePositions.[x] < pagePositions.[y]
        else
            true // as per hint in description
    )

let reorderUpdate (pageOrderingRules: PageOrderingRules) (pageUpdate: PageUpdate) =
    // Filter rules to only those involving pages in this update
    let pageSet = set pageUpdate
    let applicableRules =
        pageOrderingRules
        |> List.filter (fun (x,y) -> pageSet.Contains x && pageSet.Contains y)

    let graph =
        pageUpdate
        |> List.map (fun p ->
            let successors =
                applicableRules
                |> List.choose (fun (x,y) -> if x = p then Some y else None)
            p, successors
        )
        |> Map.ofList

    // Perform a topological sort based on applicable rules using the provided module
    topologicalSort graph

let getMiddleElement (lst: int list): int =
    List.item (lst.Length / 2) lst
    
let solvePart1 () =
    let pageOrderingRules : PageOrderingRules =
        lines
        |> List.filter (fun l -> l.Contains('|'))
        |> List.map (fun l ->
            let parts = l.Split('|')
            (int (parts[0].Trim()), int (parts[1].Trim())))
        
    let pageUpdates : PageUpdates =
            lines
            |> List.filter (fun l -> l.Contains(','))
            |> List.map (fun l -> l.Split(',') |> Array.map int |> Array.toList)
                
    let res = pageUpdates |> List.filter (isCorrectPageUpdate pageOrderingRules) |> List.map getMiddleElement |> List.sum
    
    assert (res = 5948)
    res


let solvePart2 () =
    let pageOrderingRules : PageOrderingRules =
        lines
        |> List.filter (fun l -> l.Contains('|'))
        |> List.map (fun l ->
            let parts = l.Split('|')
            (int (parts[0].Trim()), int (parts[1].Trim())))
        
    let pageUpdates : PageUpdates =
            lines
            |> List.filter (fun l -> l.Contains(','))
            |> List.map (fun l -> l.Split(',') |> Array.map int |> Array.toList)
            
    let invalidPageUpdates : PageUpdates =
            pageUpdates
            |> List.filter (fun u -> not (isCorrectPageUpdate pageOrderingRules u))
    
    
    let res =
        invalidPageUpdates |>
            List.sumBy (fun u ->
                let correctedUpdate = reorderUpdate pageOrderingRules u
                correctedUpdate |> getMiddleElement)
    
    assert (res = 3062)
    res
 
let run () =
    let part1Result = solvePart1()
    printfn $"Day 4, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 4, Part 2: %A{part2Result}"
