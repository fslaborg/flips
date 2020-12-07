namespace Flips

open SliceMap

[<AutoOpen>]
module SliceMap =
  
    [<AutoOpen>]
    type Summer () =

        /// A function for summing the contents of a SliceMap
        static member sum(x:ISliceData<'Key, Flips.Types.Decision>) : Flips.Types.LinearExpression =
            TryFind.sum x.Keys x.TryFind

        /// A function for summing the contents of a SliceMap
        static member sum(x:ISliceData<'Key, Flips.Types.LinearExpression>) =
            TryFind.sum x.Keys x.TryFind

        /// A function for summing the contents of a SliceMap
        static member sum(x:ISliceData<'Key, float>) : float =
            TryFind.sum x.Keys x.TryFind