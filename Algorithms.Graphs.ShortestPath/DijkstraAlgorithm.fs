namespace Algorithms.Graphs.ShortestPath

module Dijkstra =
  open Algorithms.Graphs.Types

  let private isPositionValid graph position =
    position.First >= 0
    && position.Second >= 0
    && Array2D.length1 graph > position.First
    && Array2D.length2 graph > position.Second

  let private shortestPath graph startPosition endPosition =
    ""

  let findShortestPath graph startPosition endPosition =
    let isValid = isPositionValid graph
    match startPosition |> isValid && endPosition |> isValid with
      | true -> Some (shortestPath graph startPosition endPosition)
      | false  -> None
