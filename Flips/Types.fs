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

    override this.ToString() =
        sprintf "%A" data


type DenseMatrix (values:float [,]) =
    let mutable data = values

    new(rowCount:int, colCount:int) =
        DenseMatrix(Array2D.zeroCreate rowCount colCount)

    member this.Item
        with get(rowIdx:int, colIdx:int) = data.[rowIdx, colIdx]
        and set (rowIdx, colIdx) value = data.[rowIdx, colIdx] <- value

    member this.GetLength dimension =
        data.GetLength dimension
        
    member this.UpdateColumn(colIdx: int, column:DenseVector) =
        for i = 0 to data.GetLength(1) do
            data.[i, colIdx] <- column.[i]

    override this.ToString() =
        sprintf "%A" data




type Vector =
    | Dense of DenseVector
    override this.ToString() =
        match this with
        | Dense v -> sprintf "%O" v


type Matrix =
    | Dense of DenseMatrix


type Factorization =
    | DenseLU of DenseMatrix




