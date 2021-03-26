namespace Algorithms.MazeRouting
  module Types =
    type Maze = int[,]

    [<Struct>]
    type Point = {
      X: int;
      Y: int;
    }