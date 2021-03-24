namespace DiffTool.Services
  module DiffTool =
    open Algorithms.EditDistance
    open Algorithms.Graphs.Types
    open Algorithms.Graphs.ShortestPath

    let findDifferences first second =
          let diff = WagnerFisher.buildDifferenceMatrix 1 1 1 first second

          let path =
            ({First=0; Second=0}, {First=0; Second=0})
            ||> Dijkstra.findShortestPath diff

          diff
