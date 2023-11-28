namespace DiffTool

module Diff =
    let private vagnerFisher<'a when 'a : equality> (firstArray: 'a array) (secondArray: 'a array) =
        let editingMatrix = Array2D.init (firstArray.Length + 1) (secondArray.Length + 1) (fun _ _ -> 0)
        
        for i in 1 .. firstArray.Length + 1 do
            for j in 1 .. secondArray.Length + 1 do
                if (firstArray[i-1] = secondArray[j-1]) then
                    Array2D.set editingMatrix i j editingMatrix[i-1, j-1]
                else
                    let insertion = editingMatrix[i-1, j]
                    let deletion = editingMatrix[i, j-1]
                    let substitution = editingMatrix[i-1, j-1]
                    Array2D.set editingMatrix i j (min insertion (min deletion substitution))
                    
        editingMatrix