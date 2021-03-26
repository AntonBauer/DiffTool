module Utils
  let flip f x y = f y x

  let seqValidator values aggregator validator =
    values
      |> Seq.map validator
      |> Seq.reduce aggregator