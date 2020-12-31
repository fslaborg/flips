[<AutoOpen>]
module internal Utility

open System.Collections.Generic


[<RequireQualifiedAccess>]
module Map =

    let ofDictionary (d: Dictionary<_, _>) =
        d
        |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
        |> Map.ofSeq