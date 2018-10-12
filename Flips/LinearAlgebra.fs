module Flips.LinearAlgebra

open Flips.Types

module Matrix =

    module Dense =

        let createLU (matrix:DenseMatrix) =

            let n = matrix.GetLength(0)
            let lu = DenseMatrix(n, n)

            // Copy the values from the origin matrix
            for rowIdx = 0 to n-1 do
                for colIdx = 0 to n-1 do
                    lu.[rowIdx, colIdx] <- matrix.[rowIdx, colIdx]

            for workingIdx = 0 to n-2 do
                for rowIdx = workingIdx+1 to n-1 do
                    let pivotValue = lu.[rowIdx, workingIdx] / lu.[workingIdx, workingIdx]
                    for colIdx = workingIdx to n-1 do
                        if colIdx = workingIdx then
                            lu.[rowIdx, colIdx] <- pivotValue
                        else
                            lu.[rowIdx, colIdx] <- lu.[rowIdx, colIdx] - pivotValue * lu.[workingIdx, colIdx]
                            

            Factorization.DenseLU lu


module Dense =

    let luSolve (lu:DenseMatrix) (rhs:DenseVector) =
        let n = rhs.GetLength(0)
        let y = DenseVector n
        let x = DenseVector n
        let mutable sum = 0.0

        // Solve Ly = b
        for i = 0 to n-1 do
            sum <- 0.0
            for k = 0 to i-1 do
                sum <- sum + lu.[i, k] * y.[k]
            y.[i] <- rhs.[i] - sum

        // Solve Ux = y
        for i = n-1 downto 0 do
            sum <- 0.0
            for k = i+1 to n-1 do
                sum <- sum + lu.[i, k] * x.[k]
            x.[i] <- (1.0 / lu.[i, i]) * (y.[i] - sum)

        Vector.Dense x


let factorize (matrix:Matrix) =
    match matrix with
    | Dense m -> Matrix.Dense.createLU m

let solve (A:Factorization) (rhs:Vector) =
    match A, rhs with
    | DenseLU lu, Vector.Dense v -> Dense.luSolve lu v