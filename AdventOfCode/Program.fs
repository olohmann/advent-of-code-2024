open System

[<EntryPoint>]
let main argv =
    let dayToRun =
        if argv.Length > 0 then
            match Int32.TryParse(argv.[0]) with
            | (true, num) when num >= 1 && num <= 25 -> num
            | _ -> 1
        else
            1

    match dayToRun with
    | 1 -> AdventOfCode.Days.Day01.run()
    // Add more days as implemented
    | _ -> printfn "Day %d is not implemented yet." dayToRun

    0 // Return an integer exit code
