// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open DiffTool.Services

let private printItem _ j item =
    if j = 0 then printfn "%s" "\n\r"
    printf "%i " item

[<EntryPoint>]
let main argv =
    (Seq.toArray "Kitten", Seq.toArray "Sitting")
    ||> DiffTool.findDifferences
    |> Array2D.iteri printItem
    0