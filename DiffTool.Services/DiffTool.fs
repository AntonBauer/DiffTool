namespace DiffTool.Services
  module DiffTool =
    open Algorithms.EditDistance
    open Algorithms.MazeRouting
    open Algorithms.MazeRouting.Types

    let private getStartPoint diff =
      {X = Array2D.length1 diff - 1; Y = Array2D.length2 diff - 1}

    let private endPoint = {X = 0; Y = 0}

    let findDifferences first second =
      let diff = WagnerFisher.buildDifferenceMatrix 1 1 1 first second
      
      (getStartPoint diff, endPoint)
      ||> LeeAlgorithm.tracePath diff
