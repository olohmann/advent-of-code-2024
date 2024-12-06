module AdventOfCode.Utils.Input

open Board2D
open System.IO
open AdventOfCode.Utils.Types
open System.Text.RegularExpressions

type DataInput =
    | TestData
    | ExampleData

let replaceMultipleWhitespaces (input: string) : string =
    Regex.Replace(input, @"\s+", " ")

let getInputFilePath (fileType: DataInput) (day: AdventDays) : string =
    let baseDir = Directory.GetCurrentDirectory()
    let fileName = 
        match fileType with
        | TestData -> $"{day.ToString()}.txt"
        | ExampleData -> $"{day.ToString()}-Example.txt"
    Path.Combine(baseDir, "Inputs", fileName)

let readLines (day: AdventDays) (fileType: DataInput) : string array =
    File.ReadAllLines (getInputFilePath fileType day)

let readText (day: AdventDays) (fileType: DataInput)  : string =
    File.ReadAllText (getInputFilePath fileType day)
    
let parseToCharBoard (day: AdventDays) (fileType: DataInput) : Board2D<char> =
    let lines = readLines day fileType
    let res = lines |> Array.map (fun line -> line.ToCharArray())
    res
    
let readLinesAs2DArray<'T> (chompWhitespace: bool) (splitOption: string) (parseFunc: string -> 'T) (day: AdventDays) (fileType: DataInput): 'T[][] =
    let lines = readLines day fileType 
    lines |> Array.map (fun line -> if chompWhitespace then replaceMultipleWhitespaces line else line)
          |> Array.map (fun line -> line.Split([|splitOption|], System.StringSplitOptions.None))
          |> Array.map (Array.map parseFunc)

let inputAs2DArraySplitBySpace (parseFunc: string -> 'T) (day: AdventDays) (fileType: DataInput) : 'T[][] =
    readLinesAs2DArray true " " parseFunc day fileType