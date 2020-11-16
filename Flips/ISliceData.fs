namespace Flips.SliceMap


type ISliceData<'Key, 'Value when 'Key : comparison and 'Value : equality> =
    abstract member Keys : 'Key seq
    abstract member TryFind : TryFind<'Key, 'Value>


[<AutoOpen>]
module Sum =

    open Flips.SliceMap

    /// <summary>A function which sums the values contained in a SliceMap</summary>
    /// <param name="x">An instance of ISliceData</param>
    /// <returns>A LinearExpression</returns>
    let inline sum (x:ISliceData<'Key, 'Value>) : Flips.Types.LinearExpression =
        TryFind.sum x.Keys x.TryFind

    /// <summary>A function which sums a sequence of SliceMaps</summary>
    /// <param name="x">A sequence of SliceMaps</param>
    /// <returns>A LinearExpression</returns>
    let inline sumAll< ^a, ^b when ^a: (static member Sum: ^a -> Flips.Types.LinearExpression)
                              and ^a: (static member (+): ^a * ^a -> ^a)
                              and ^a: (static member Zero: ^a)> (k1: ^a seq) : Flips.Types.LinearExpression =
        let r = Seq.sum k1
        ((^a) : (static member Sum: ^a -> Flips.Types.LinearExpression) r)