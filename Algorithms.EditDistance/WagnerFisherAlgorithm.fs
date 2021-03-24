namespace Algorithms.EditDistance

  module WagnerFisher =
    open Microsoft.FSharp.Collections

    let private insertCost = 1
    let private deleteCost = 1
    let private substitiuteCost = 1

    let diffInitializer i j =
      match (i, j) with
        | (0, 0) -> 0
        | (0, value) -> value
        | (value, 0) -> value
        | (_, _) -> 0

    let private calculateSubstituteCost (firstItem: 'T) (secondItem: 'T) =
      if firstItem = secondItem then 0 else substitiuteCost

    let private initCell (firstArray: 'T[]) (secondArray: 'T[]) (diffMatrix: int[,]) i j _ =
      if i = 0 || j = 0 then ignore()
      else
        Array.min [|
          diffMatrix.[i-1, j] + insertCost;
          diffMatrix.[i, j-1] + deleteCost;
          diffMatrix.[i-1, j-1] + calculateSubstituteCost firstArray.[i-1] secondArray.[j-1]
        |]
        |> Array2D.set diffMatrix i j


    let buildDifferenceMatrix (firstArray: 'T[]) (secondArray: 'T[]) =
      let diffMatrix = Array2D.init (Array.length firstArray + 1) (Array.length secondArray + 1) diffInitializer
      
      (initCell firstArray secondArray diffMatrix, diffMatrix)
      ||> Array2D.iteri
      
      diffMatrix