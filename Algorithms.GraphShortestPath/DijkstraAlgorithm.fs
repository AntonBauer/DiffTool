namespace Algorithms.Graphs.ShortestPath

module Dijkstra =
  open Algorithms.Graphs.Types

  let private isPositionValid (graph: Graph) (position: Position) =
    position.First >= 0
    && position.Second >= 0
    && Array2D.length1 graph > position.First
    && Array2D.length2 graph > position.Second

  let private shortestPath (graph: Graph) (startPosition: Position) (endPosition: Position) =
    ""

  let findShortestPath graph startPosition endPosition =
    let isValid = isPositionValid graph
    if startPosition |> isValid && endPosition |> isValid then
      Some (shortestPath graph startPosition endPosition)
    else
      None
