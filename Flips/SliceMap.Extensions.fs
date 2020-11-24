namespace Flips

open SliceMap

[<AutoOpen>]
module SliceMap =
  
  /// <summary>A function which sums the values contained in a SliceMap</summary>
  /// <param name="x">An instance of ISliceData</param>
  /// <returns>A LinearExpression with a Unit of Measure</returns>
  let inline sum (x:ISliceData<'Key, 'Value>) : Flips.Types.LinearExpression =
      TryFind.sum x.Keys x.TryFind
