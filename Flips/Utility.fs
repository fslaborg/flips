[<AutoOpen>]
module internal Utility

open System.Collections.Generic


let flip23 f a c b = f a b c


[<RequireQualifiedAccess>]
module Map =

    let ofDictionary (d: Dictionary<_, _>) =
        d
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Map.ofSeq


[<RequireQualifiedAccess>]
module ByRefPair =

    let toOption (b, a) =
        if b then Some a else None


[<RequireQualifiedAccess>]
module Dictionary =

    let tryFind key (d: Dictionary<_, _>) =
        key |> d.TryGetValue |> ByRefPair.toOption