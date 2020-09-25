namespace Flips.SliceMap


type ISliceData<'Key, 'Value when 'Key : comparison and 'Value : equality> =
    abstract member Keys : 'Key seq
    abstract member TryFind : TryFind<'Key, 'Value>


[<AutoOpen>]
module Sum =

    open Flips.SliceMap

    /// A type to supprt the `sum SliceMap` syntax
    type internal Summer () =

        static member inline Sum (x:ISliceData<_,_>) : Flips.Types.LinearExpression =
          TryFind.sum x.Keys x.TryFind

        static member Sum (x:ISliceData<_,Flips.Types.Decision>) : Flips.Types.LinearExpression =
          let newTryFind = x.TryFind >> Option.map (fun v -> 1.0 * v)
          TryFind.sum x.Keys newTryFind


    /// <summary>A function which sums the values contained in a SliceMap</summary>
    /// <param name="x">An instance of ISliceData</param>
    /// <returns>A LinearExpression</returns
    let inline sum (x:ISliceData<'Key, 'Value>) : Flips.Types.LinearExpression =
        TryFind.sum x.Keys x.TryFind

    /// <summary>A function which sums a sequence of SliceMaps</summary>
    /// <param name="x">A sequence of SliceMaps</param>
    /// <returns>A LinearExpression</returns
    let inline sumAll< ^a, ^b when ^a: (static member Sum: ^a -> Flips.Types.LinearExpression) 
                              and ^a: (static member (+): ^a * ^a -> ^a)
                              and ^a: (static member Zero: ^a)> (k1: ^a seq) : Flips.Types.LinearExpression = 
        let r = Seq.sum k1
        ((^a) : (static member Sum: ^a -> Flips.Types.LinearExpression) r)