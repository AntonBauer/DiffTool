namespace Algorithms.Graphs
  module Types =
    
    type Graph = int[,]
    
    [<Struct>]
    type Position = 
      {
        First: int
        Second: int
      }
