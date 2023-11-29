namespace DiffTool

module Diff =
    type Substitution = int * int

    type DiffResult =
        { Insertions: int array
          Deletions: int array
          Substitutions: Substitution array }

    type VagnerFisher<'a when 'a: equality> = 'a array -> 'a array -> int array2d
    type Lee = int array2d -> DiffResult
    type Diff<'a when 'a: equality> = 'a array -> 'a array -> DiffResult

    let vagnerFisher: VagnerFisher<'a> =
        fun firstArray secondArray ->
            let diffMatrix =
                Array2D.zeroCreate (firstArray.Length + 1) (secondArray.Length + 1)

            for i in 1 .. firstArray.Length + 1 do
                for j in 1 .. secondArray.Length + 1 do
                    if (firstArray[i - 1] = secondArray[j - 1]) then
                        Array2D.set diffMatrix i j diffMatrix[i - 1, j - 1]
                    else
                        let insertion = diffMatrix[i - 1, j]
                        let deletion = diffMatrix[i, j - 1]
                        let substitution = diffMatrix[i - 1, j - 1]
                        Array2D.set diffMatrix i j (min insertion (min deletion substitution))

            diffMatrix

    let lee: Lee =
        fun diffMatrix ->
            let tst =
                { Insertions = Array.empty
                  Deletions = Array.empty
                  Substitutions = Array.empty }

            tst
            
    let diff: Diff<'a> = fun firstArray secondArray ->
        vagnerFisher firstArray secondArray
        |> lee
