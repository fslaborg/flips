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
        s 
        |> Seq.map (fun (k, v) -> KeyValuePair(k, v)) 
        |> Dictionary
        |> ofDictionary

    let toSeq (keys:seq<_>) (s:TryFind<_,_>) =
        let lookup k = s k |> Option.map (fun v -> k, v)
        
        keys
        |> Seq.choose lookup

    let toMap keys (s:TryFind<_,_>) =
        s |> (toSeq keys) |> Map.ofSeq

    let inline sum keys (tryFind:TryFind<_,_>) =
        let mutable acc = LanguagePrimitives.GenericZero

        for k in keys do
            match tryFind k with
            | Some v -> 
                acc <- acc + v
            | None -> ()

        acc












