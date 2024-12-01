module AdventOfCode.Utils.Input

open System.IO
open AdventOfCode.Utils.Types

type InputFileType =
    | Regular
    | Example

let getInputFilePath (fileType: InputFileType) (day: AdventDays) : string =
    let baseDir = Directory.GetCurrentDirectory()
    let fileName = 
        match fileType with
        | Regular -> $"{day.ToString()}.txt"
        | Example -> $"{day.ToString()}-Example.txt"
    Path.Combine(baseDir, "Inputs", fileName)

let readLines (fileType: InputFileType) (day: AdventDays) : string array =
    File.ReadAllLines (getInputFilePath fileType day)

let readText (fileType: InputFileType) (day: AdventDays) : string =
    File.ReadAllText (getInputFilePath fileType day)
    
let readLinesAs2DArray (fileType: InputFileType) (day: AdventDays) (splitOption: string) : string[][] =
    let lines = readLines fileType day
    lines |> Array.map (fun line -> line.Split([|splitOption|], System.StringSplitOptions.None))
