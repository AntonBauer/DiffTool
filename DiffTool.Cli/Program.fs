// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Algorithms.EditDistance

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let private printItem _ j item =
    if j = 0 then printfn "%s" "\n\r"
    printf "%i " item

[<EntryPoint>]
let main argv =
    WagnerFisher.buildDifferenceMatrix 1 1 1 (Seq.toArray "Kitten") (Seq.toArray "Sitting")
    |> Array2D.iteri printItem
    0