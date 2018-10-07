module Flips.Types


type DenseVector (values:float []) =
    let mutable data = values

    new(n:int) =
        DenseVector(Array.zeroCreate<float> n)
        
    member this.Item
        with get(index) = data.[index]
        and set index value = data.[index] <- value

    member this.GetLength(n) =
        data.GetLength(n)


type DenseMatrix (values:float [,]) =
    let mutable data = values

    new(rowCount:int, colCount:int) =
        DenseMatrix(Array2D.zeroCreate rowCount colCount)

    member this.Item
        with get(rowIdx:int, colIdx:int) = data.[rowIdx, colIdx]
        and set (rowIdx, colIdx) value = data.[rowIdx, colIdx] <- value

    member this.GetLength dimension =
        data.GetLength dimension




type Vector =
    | Dense of DenseVector


type Matrix =
    | Dense of DenseMatrix


type Factorization =
    | DenseLU of DenseMatrix




