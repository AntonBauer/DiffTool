namespace Algorithms.EditDistance
  module WagnerFisher =

    let private insertCost = 1
    let private deleteCost = 1
    let private substitiuteCost = 1

    let diffInitializer i j =
      match (i, j) with
        | (0, 0) -> 0
        | (0, value) -> value
        | (value, 0) -> value
        | (_, _) -> 0

    let private calculateSubstituteCost firstItem secondItem =
      match firstItem = secondItem with
       | true -> 0
       | false -> substitiuteCost

    let private calculateCellValue firstArray secondArray diffMatrix i j =
      Array.min [|
        Array2D.get diffMatrix (i-1) j + insertCost
        Array2D.get diffMatrix i (j-1) + deleteCost
        Array2D.get diffMatrix (i-1) (j-1) + calculateSubstituteCost (Array.get firstArray (i-1)) (Array.get secondArray (j-1))
      |]

    let private initCell firstArray secondArray diffMatrix i j _ =
      match i = 0 || j = 0 with 
        | true -> ignore()
        | false ->
          calculateCellValue firstArray secondArray diffMatrix i j
          |> Array2D.set diffMatrix i j


    let buildDifferenceMatrix firstArray secondArray =
      let diffMatrix = Array2D.init (Array.length firstArray + 1) (Array.length secondArray + 1) diffInitializer
      
      (initCell firstArray secondArray diffMatrix, diffMatrix)
      ||> Array2D.iteri
      
      diffMatrix