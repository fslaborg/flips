module Math

open Flips.Types

module Matrix =

    module Dense =

        let createLU (matrix:DenseMatrix) =

            let n = matrix.GetLength(0)
            let lu = DenseMatrix(n, n)
            let mutable sum = 0.0

            for i = 0 to n-1 do // Index across each row
                for j = i to n-1 do // Index from Col = Row to the end. This will be diagnoal
                    sum <- 0.0  // For each column we are going to be keeping track of a sum

                    for k = 0 to i-1 do // Index from the beginning of the matrix to the value of the row index
                        sum <- sum + lu.[i, k] * lu.[k, j]

                    lu.[i, j] <- matrix.[i, j] - sum // This is setting the value of U upper matrix
                    printfn "%A" lu

                for j = i+1 to n-1 do // Index the Columns from Row Index + 1 to the right side of the matrix
                    sum <- 0.0 // Start a summation for the L matrix entry

                    for k = 0 to i-1 do
                        sum <- sum + lu.[j, k] * lu.[k, i]

                    printfn "i:%A j:%A" i j
                    lu.[j, i] <- (1.0 / lu.[i, i]) * (matrix.[j, i] - sum) // This is setting the value of the L lower matrix
                    printfn "%A" lu

            Factorization.DenseLU lu


module LinearSystem =

    module internal Dense =
    
        let luSolve (lu:DenseMatrix) (rhs:DenseVector) =
            let n = rhs.GetLength(0)
            let y = Array.zeroCreate n
            let x = Array.zeroCreate n
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

            x


    let solve (A:Factorization) (rhs:Vector) =
        match A, rhs with
        | DenseLU lu, Vector.Dense v -> Dense.luSolve lu v