module AdventOfCode.Utils.Output

let prettyPrint2DArray (array: string[][]) =
    array
    |> Array.map (fun row -> String.concat ";" row)
    |> String.concat "\n"
    |> printfn "%s"
   
let prettyPrintArray (array: 'T[]) =
    array
    |> Array.map (fun elem -> elem.ToString())
    |> String.concat ", "
    |> printfn "%s"