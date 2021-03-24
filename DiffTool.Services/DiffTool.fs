namespace DiffTool.Services
  module DiffTool =
    open Algorithms.EditDistance

    let findDifferences first second =
          let diff = WagnerFisher.buildDifferenceMatrix 1 1 1 first second
          diff
