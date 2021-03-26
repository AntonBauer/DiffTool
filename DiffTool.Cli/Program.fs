// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open DiffTool.Services
open Algorithms.MazeRouting.Types

let private printPoint point =
    printf "%i %i, " point.X point.Y

let private printPath path =
    match path with
    | Some p -> List.iter printPoint p
    | None -> printfn "Not today, boy"

[<EntryPoint>]
let main argv =
    (Seq.toArray "Kitten", Seq.toArray "Sitting")
    ||> DiffTool.findDifferences
    |> printPath
    0