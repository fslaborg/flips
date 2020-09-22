namespace Flips.SliceMap

[<AutoOpen>]
module Sum =

    //type DefaultSummer () =
    //    static member inline Sum (x:ISliceData<_,_>) =
    //        TryFind.sum x.Keys x.TryFind

    type internal Summer () =

      //inherit DefaultSummer ()
      static member inline Sum (x:ISliceData<_,_>) : Flips.Types.LinearExpression =
        TryFind.sum x.Keys x.TryFind

      //static member Sum (x:ISliceData<_,Flips.Types.LinearExpression>) : Flips.Types.LinearExpression =
      //  TryFind.sum x.Keys x.TryFind

      //static member Sum (x:ISliceData<_,Flips.UnitsOfMeasure.Types.LinearExpression<_>>) : Flips.UnitsOfMeasure.Types.LinearExpression<_> =
      //  TryFind.sum x.Keys x.TryFind

      static member Sum (x:ISliceData<_,Flips.Types.Decision>) : Flips.Types.LinearExpression =
        let newTryFind = x.TryFind >> Option.map (fun v -> 1.0 * v)
        TryFind.sum x.Keys newTryFind

      //static member Sum (x:ISliceData<_,Flips.UnitsOfMeasure.Types.Decision<_>>) : Flips.UnitsOfMeasure.Types.LinearExpression<_> =
      //  let newTryFind = x.TryFind >> Option.map (fun v -> 1.0 * v)
      //  TryFind.sum x.Keys newTryFind


    let inline sum (x:ISliceData<'Key, 'Value>) : Flips.Types.LinearExpression =
        TryFind.sum x.Keys x.TryFind

    let inline sumAll< ^a, ^b when ^a: (static member Sum: ^a -> Flips.Types.LinearExpression) 
                              and ^a: (static member (+): ^a * ^a -> ^a)
                              and ^a: (static member Zero: ^a)> (k1: ^a seq) : Flips.Types.LinearExpression = 
        let r = Seq.sum k1
        ((^a) : (static member Sum: ^a -> Flips.Types.LinearExpression) r)