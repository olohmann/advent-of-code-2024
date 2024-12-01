module AdventOfCode.Utils.Misc

let getColumn (columnIndex: int) (array: 'a[][]) : 'a[] =
    array |> Array.map (fun row -> row.[columnIndex])