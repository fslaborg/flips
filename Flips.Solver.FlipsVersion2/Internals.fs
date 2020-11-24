namespace Flips.FlipsVersion2.Internals

open System.Collections.Generic

module internal Dictionary =

    let tryFind k (d:Dictionary<_,_>) =
        match d.TryGetValue k with
        | true, v -> Some v
        | false, _ -> None

