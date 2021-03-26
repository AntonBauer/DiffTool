namespace Algorithms.MazeRouting
  module LeeAlgorithm =
    open Utils
    open Types

    let private isInside maze point =
      point.X >= 0
      && Array2D.length1 maze > point.X
      && point.Y >= 0
      && Array2D.length2 maze > point.Y

    let private extractNeighbours extractor currentPosition =
      (
        extractor (currentPosition.X - 1) currentPosition.Y,
        extractor currentPosition.X (currentPosition.Y - 1),
        extractor (currentPosition.X - 1) (currentPosition.Y - 1)
      )

    let private determineNext currentPosition extractNeighbours =
      let (top, left, topLeft) = extractNeighbours currentPosition
      let min = Array.min [|top; left; topLeft|]

      if min = top then {currentPosition with X = currentPosition.X - 1}
      elif min = left then {currentPosition with Y = currentPosition.Y - 1}
      else {currentPosition with X = currentPosition.X - 1; Y = currentPosition.Y - 1}

    let findNextPoint extractor currentPosition =
      extractNeighbours extractor
      |> determineNext currentPosition

    let rec private trace mazeValueExtractor finish currentPosition path =
      if currentPosition = finish then path
      else path @ [currentPosition] |> trace mazeValueExtractor finish (findNextPoint mazeValueExtractor currentPosition)

    let tracePath (maze: Maze) from finish =
      let pointsValid = 
        isInside maze
        |> seqValidator [|from; finish|] (&&)
      
      if not pointsValid then None
      else Some (trace (Array2D.get maze) finish from [])
