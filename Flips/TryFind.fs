namespace Flips.SliceMap

open System.Collections.Generic


type TryFind<'Key, 'Value> = 'Key -> 'Value option


module TryFind =

    let ofDictionary (d:Dictionary<'Key,'Value>) : TryFind<'Key, 'Value> =
        fun k -> 
          match d.TryGetValue(k) with
          | true, value -> Some value
          | false, _ -> None

    let ofSeq (s:seq<'Key * 'Value>) : TryFind<'Key, 'Value> =
        let d = Dictionary ()
        
        for (k, v) in s do
                d.[k] <- v

        ofDictionary d

    let toSeq (keys:seq<_>) (s:TryFind<_,_>) =
        let lookup k = s k |> Option.map (fun v -> k, v)
        
        keys
        |> Seq.choose lookup

    let toMap keys (s:TryFind<_,_>) =
        s |> (toSeq keys) |> Map.ofSeq

    let equals keys (a:TryFind<_,_>) (b:TryFind<_,_>) =
        let mutable result = true

        for k in keys do
            let aValue = a k
            let bValue = b k
            if aValue <> bValue then
                result <- false

        result


    let inline sum keys (tryFind:TryFind<_,_>) =
        let mutable acc = LanguagePrimitives.GenericZero

        for k in keys do
            match tryFind k with
            | Some v -> 
                acc <- acc + v
            | None -> ()

        acc












