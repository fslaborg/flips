module Flips.LinearAlgebra

open Flips.Types

module Vector =

    let internal swapValues idx1 idx2 (vector: 'T []) =
        let value1 = vector.[idx1]
        vector.[idx1] <- vector.[idx2]
        vector.[idx2] <- value1

    let internal rowPermute (RowPermutation.Dense p) (vector: 'T []) =
        let n = Array.length vector
        let newVector = Array.zeroCreate n

        for i = 0 to n-1 do
            newVector.[i] <- vector.[p.[i]]

        newVector

module Matrix =

    open Vector

    module Dense =

        let private swapRows idx1 idx2 (arr:'T [,]) =
            let n = Array2D.length2 arr
            let row1 = arr.[idx1,*]
            for j = 0 to n-1 do
                arr.[idx1, j] <- arr.[idx2, j]
                arr.[idx2, j] <- row1.[j]


        let private indexOfMaxValueInColumnStartingAt colIdx startingIdx (matrix:float [,]) =
            let n = Array2D.length2 matrix
            let mutable maxIndex = startingIdx
            let mutable maxValue = matrix.[startingIdx, colIdx]

            for i = startingIdx to n-1 do
                if matrix.[i, colIdx] > maxValue then
                    maxIndex <- i
                    maxValue <- matrix.[i, colIdx]

            maxIndex

        let createLU (matrix:float [,]) =

            let n = matrix.GetLength(0)
            let lu = Array2D.copy matrix

            for workingIdx = 0 to n-2 do
                for rowIdx = workingIdx+1 to n-1 do
                    let pivotValue = lu.[rowIdx, workingIdx] / lu.[workingIdx, workingIdx]
                    for colIdx = workingIdx to n-1 do
                        if colIdx = workingIdx then
                            lu.[rowIdx, colIdx] <- pivotValue
                        else
                            lu.[rowIdx, colIdx] <- lu.[rowIdx, colIdx] - pivotValue * lu.[workingIdx, colIdx]
                            

            Factorization.DenseLU lu

        let createPLU (matrix:float [,]) =
            let n = Array2D.length2 matrix
            let lu = Array2D.copy matrix
            let p = [|0..n-1|]

            for k = 0 to n-1 do
                let maxValueRowIdx = indexOfMaxValueInColumnStartingAt k k matrix
                if k <> maxValueRowIdx then
                    swapRows k maxValueRowIdx lu
                    swapValues k maxValueRowIdx p

                for rowIdx = k+1 to n-1 do
                    let pivotValue = lu.[rowIdx, k] / lu.[k, k]
                    for colIdx = k to n-1 do
                        if colIdx = k then
                            lu.[rowIdx, colIdx] <- pivotValue
                        else
                            lu.[rowIdx, colIdx] <- lu.[rowIdx, colIdx] - pivotValue * lu.[k, colIdx]

            Factorization.DensePLU (RowPermutation.Dense p, lu)

module Solve =

    let luSolve (lu:float [,]) (rhs:float []) =
        let n = rhs.GetLength(0)
        let y = Array.zeroCreate n
        let x = Array.zeroCreate n
        let mutable sum = 0.0

        // Solve Ly = b
        for i = 0 to n-1 do
            sum <- 0.0
            for k = 0 to i-1 do
                sum <- sum + y.[k] * lu.[i, k]
            y.[i] <- rhs.[i] - sum


        // Solve Ux = y
        for i = n-1 downto 0 do
            sum <- 0.0
            for k = n-1 downto i do
                sum <- sum + x.[k] * lu.[i, k]
            x.[i] <- (y.[i] - sum) / lu.[i, i]

        x

    let pluSolve p (lu:float [,]) (rhs:float []) =
        let rhs' = Vector.rowPermute p rhs
        luSolve lu rhs'

let factorize (matrix:Matrix) =
    match matrix with
    | Matrix.Dense m -> Matrix.Dense.createPLU m

let solve (A:Factorization) (rhs:Vector) =
    match A, rhs with
    | DenseLU lu, Vector.Dense v -> Solve.luSolve lu v
    | DensePLU (p, lu), Vector.Dense v -> Solve.pluSolve p lu v