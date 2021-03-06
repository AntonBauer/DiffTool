namespace Algorithms.EditDistance
  module WagnerFisher =
    open Utils

    let private diffInitializer i j =
      match (i, j) with
        | (0, 0) -> 0
        | (0, value) -> value
        | (value, 0) -> value
        | (_, _) -> 0

    let private initDiff first second =
      Array2D.init (Array.length first + 1) (Array.length second + 1) diffInitializer

    let private calculateSubstituteCost substituteCost first second =
      match first = second with
       | true -> 0
       | false -> substituteCost

    let private calculateCellValue (first: 'a []) second insertCost deleteCost calculateSubstituteCost diff i j =
      Array.min [|
        Array2D.get diff (i-1) j + deleteCost
        Array2D.get diff i (j-1) + insertCost
        Array2D.get diff (i-1) (j-1) + calculateSubstituteCost (Array.get first (i-1)) (Array.get second (j-1))
      |]

    let private calculateCell cellValueCalculator diff i j _ =
        match i = 0 || j = 0 with 
          | true -> ignore()
          | false ->
            cellValueCalculator diff i j
            |> Array2D.set diff i j

    let private fillDiff diff cellValueCalclulator =
      cellValueCalclulator diff
      |> flip Array2D.iteri diff

      diff
      
    let buildDifferenceMatrix insertCost deleteCost substituteCost firstArray secondArray =
      calculateSubstituteCost substituteCost
      |> calculateCellValue firstArray secondArray insertCost deleteCost
      |> calculateCell
      |> fillDiff (initDiff firstArray secondArray)
