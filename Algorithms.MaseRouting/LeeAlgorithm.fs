namespace Algorithms.MazeRouting
  module LeeAlgorithm =
    open Types

    let private isPointValid maze point =
      point.X >= 0
      && Array2D.length1 maze > point.X
      && point.Y >= 0
      && Array2D.length2 maze > point.Y

    let private arePointsInMaze maze from finish =
      let isValid = isPointValid maze
      (isValid from, isValid finish)
      ||> (&&)

    let private extractNeighbours extractValue currentPosition =
      (
        extractValue (currentPosition.X - 1) currentPosition.Y,
        extractValue currentPosition.X (currentPosition.Y - 1),
        extractValue (currentPosition.X - 1) (currentPosition.Y - 1)
      )

    let private determineNext currentPosition extractNeighbours =
      let (top, left, topLeft) = extractNeighbours currentPosition
      let min = Array.min [|top; left; topLeft|]

      if min = top then {currentPosition with X = currentPosition.X - 1}
      elif min = left then {currentPosition with Y = currentPosition.Y - 1}
      else {currentPosition with X = currentPosition.X - 1; Y = currentPosition.Y - 1}


    let findNextPoint extractValue currentPosition =
      extractNeighbours extractValue
      |> determineNext currentPosition

    let rec private findPath maze finish currentPosition path =
      let mazeValueExtractor = Array2D.get maze

      if currentPosition = finish then path
      else path @ [currentPosition]
           |> findPath maze finish (findNextPoint mazeValueExtractor currentPosition)

    let private trace maze from finish =
      findPath maze finish from List.empty

    let tracePath (maze: Maze) from finish =
      if arePointsInMaze maze from finish then Some (trace maze from finish)
      else None
