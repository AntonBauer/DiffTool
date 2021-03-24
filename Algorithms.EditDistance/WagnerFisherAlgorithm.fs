namespace Algorithms.EditDistance

  module WagnerFisher =
    open Microsoft.FSharp.Collections

    let private diffInitializer i j =
      if i = 0 && j = 0 then 0
      elif j = 0 then i
      elif i = 0 then j
      else 0

    let private initCell (firstArray: 'T[]) (secondArray: 'T[]) (diffMatrix: int[,]) i j _ =
      if i = 0 || j = 0 then ignore()
      else
        let changeCost = if firstArray.[i-1] = secondArray.[j-1] then 0 else 1
        // insertion; deletion; substitution
        Array.min [|diffMatrix.[i-1, j] + 1; diffMatrix.[i, j-1] + 1; diffMatrix.[i-1, j-1] + changeCost|]
        |> Array2D.set diffMatrix i j


    let buildDifferenceMatrix (firstArray: 'T[]) (secondArray: 'T[]) =
      let diffMatrix = Array2D.init (Array.length firstArray + 1) (Array.length secondArray + 1) diffInitializer
      
      (initCell firstArray secondArray diffMatrix, diffMatrix)
      ||> Array2D.iteri
      
      diffMatrix