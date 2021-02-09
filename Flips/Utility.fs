[<AutoOpen>]
module internal Utility

open System.Collections.Generic


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


[<RequireQualifiedAccess>]
module Math =

    let kahanSum (xs: float list) =
        let rec kahanSumAux (xs: float list) (sum: float) (c: float) =
            match xs with
            | [] -> sum
            | x::xs ->
            let y = x - c in
            let t = sum + y in
            let c = (t - sum) - y in
            kahanSumAux xs t c

        match xs with
        | [] -> 0.0
        | _ -> kahanSumAux xs 0.0 0.0