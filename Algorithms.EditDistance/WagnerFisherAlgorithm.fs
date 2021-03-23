namespace Algorithms.EditDistance

  module WagnerFisher =
    open Microsoft.FSharp.Collections

    let private diffInitializer i j =
      if i = 0 && j = 0 then 0
      elif j = 0 then i
      elif i = 0 then j
      else 0

    let private initCell (firstArray: array<'T>) (secondArray: array<'T>) (diffMatrix: int[,]) i j _ =
      if i = 0 || j = 0 then ignore()
      else
        let changeCost = if firstArray.[i-1] = secondArray.[j-1] then 0 else 1
        // deletion; insertion; substitution
        Array.min [|diffMatrix.[i-1, j] + 1; diffMatrix.[i, j-1] + 1; diffMatrix.[i-1, j-1] + changeCost|]
        |> Array2D.set diffMatrix i j


    let buildDifferenceMatrix<'T when 'T : equality> (firstArray: array<'T>) (secondArray: array<'T>) =
      let diffMatrix = Array2D.init (firstArray.Length + 1) (secondArray.Length + 1) diffInitializer
      
      diffMatrix
      |> Array2D.iteri (initCell firstArray secondArray diffMatrix)
      
      diffMatrix
