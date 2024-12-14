module AdventOfCode.Days.Day08

open AdventOfCode.Utils.Input
open AdventOfCode.Utils.Types
open System.Collections.Generic

let input = readLines Day08 TestData |> Array.toList

let solvePart1()=
    let grid = input |> List.map (fun line -> line.ToCharArray())
    let height = grid.Length
    let width = if height > 0 then grid.[0].Length else 0

    let antennasByFreq = Dictionary<char, List<int*int>>()

    for y in 0..(height-1) do
        for x in 0..(width-1) do
            let c = grid.[y].[x]
            if c <> '.' then
                if not (antennasByFreq.ContainsKey(c)) then
                    antennasByFreq.[c] <- List<int*int>()
                antennasByFreq.[c].Add(x,y)

    let inBounds (x,y) =
        x >= 0 && x < width && y >= 0 && y < height

    let antinodes = HashSet<int*int>()

    for KeyValue(freq, positions) in antennasByFreq do
        // We only produce antinodes if we have at least two antennas
        let posArr = positions.ToArray()
        let n = posArr.Length
        for i in 0..(n-2) do
            for j in (i+1)..(n-1) do
                let (xA,yA) = posArr.[i]
                let (xB,yB) = posArr.[j]

                // Antinode1 = A - (B - A) = 2A - B
                let xA1 = 2*xA - xB
                let yA1 = 2*yA - yB
                
                // Antinode2 = B + (B - A) = 2B - A
                let xA2 = 2*xB - xA
                let yA2 = 2*yB - yA

                // Check bounds and add to set if valid
                if inBounds (xA1, yA1) then
                    antinodes.Add((xA1, yA1)) |> ignore
                if inBounds (xA2, yA2) then
                    antinodes.Add((xA2, yA2)) |> ignore
    let res = antinodes.Count
    
    assert (res = 423)
    res

let solvePart2 () =
    let rec gcd a b = if b = 0 then abs a else gcd b (a % b)
    let grid = input |> List.map (fun line -> line.ToCharArray())
    let height = grid.Length
    let width = if height > 0 then grid.[0].Length else 0

    // Identify antennas by frequency
    let antennasByFreq = Dictionary<char, List<int*int>>()
    for y in 0..(height-1) do
        for x in 0..(width-1) do
            let c = grid.[y].[x]
            if c <> '.' then
                if not (antennasByFreq.ContainsKey(c)) then
                    antennasByFreq.[c] <- List<int*int>()
                antennasByFreq.[c].Add(x,y)

    let inBounds (x,y) =
        x >= 0 && x < width && y >= 0 && y < height

    let antinodes = HashSet<int*int>()

    // For each frequency, process lines
    for KeyValue(freq, positions) in antennasByFreq do
        let posArr = positions.ToArray()
        let n = posArr.Length
        if n >= 2 then
            // Keep track of processed lines for this frequency
            let processedLines = HashSet<int*int*int>()
            for i in 0..(n-2) do
                for j in (i+1)..(n-1) do
                    let (xA,yA) = posArr.[i]
                    let (xB,yB) = posArr.[j]

                    let dx = xB - xA
                    let dy = yB - yA
                    let g = gcd dx dy
                    let Nx = dx / g
                    let Ny = dy / g

                    // Normalize direction (Nx,Ny) so that Nx>0 or Nx=0 and Ny>0
                    let (Nx', Ny') =
                        if Nx < 0 then
                            (-Nx, -Ny)
                        elif Nx = 0 && Ny < 0 then
                            (-Nx, -Ny)
                        else
                            (Nx, Ny)

                    // Compute line constant C for -Ny'*x + Nx'*y = C
                    let C = (-Ny') * xA + Nx' * yA

                    if not (processedLines.Contains(Nx', Ny', C)) then
                        processedLines.Add(Nx', Ny', C) |> ignore

                        let mutable kForward = 1
                        while inBounds (xA + kForward*Nx', yA + kForward*Ny') do
                            antinodes.Add((xA + kForward*Nx', yA + kForward*Ny')) |> ignore
                            kForward <- kForward + 1

                        let mutable kBackward = -1
                        while inBounds (xA + kBackward*Nx', yA + kBackward*Ny') do
                            antinodes.Add((xA + kBackward*Nx', yA + kBackward*Ny')) |> ignore
                            kBackward <- kBackward - 1

                        // Antennas are also points
                        antinodes.Add((xA,yA)) |> ignore
                        antinodes.Add((xB,yB)) |> ignore
    let res = antinodes.Count
    
    assert (res = 1287)
    res
 
let run () =
    let part1Result = solvePart1()
    printfn $"Day 8, Part 1: %A{part1Result}"

    let part2Result = solvePart2()
    printfn $"Day 8, Part 2: %A{part2Result}"