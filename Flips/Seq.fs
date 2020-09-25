namespace Flips.SliceMap

module Seq =

    let equals a b =
        let result = 
          Seq.zip a b 
          |> Seq.forall (fun (x, y) -> x = y)
        result

