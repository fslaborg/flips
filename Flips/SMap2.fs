namespace Flips.SliceMap

open System.Collections.Generic
open Utilities

type SMap2<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Value : equality> 
    (keys1:SliceSet<'Key1>, keys2:SliceSet<'Key2>, tryFind:TryFind<('Key1 * 'Key2), 'Value>) =

    let keys1 = keys1
    let keys2 = keys2
    let possibleKeys = seq {for k1 in keys1 do for k2 in keys2 -> (k1, k2)}
    let tryFind = tryFind

    new (s:seq<('Key1 * 'Key2) * 'Value>) =
        let keys1 = s |> Seq.map (fst >> fst) |> SliceSet
        let keys2 = s |> Seq.map (fst >> snd) |> SliceSet
        let store = TryFind.ofSeq s
        SMap2 (keys1, keys2, store)

    new (m:Map<('Key1 * 'Key2), 'Value>) =
      let s = m |> Map.toSeq
      SMap2 s

    member _.Keys1 = keys1
    member _.Keys2 = keys2
    member _.PossibleKeys = possibleKeys
    member _.TryFind = tryFind

    member _.AsMap () =
        tryFind
        |> TryFind.toMap possibleKeys

    override this.ToString () = 
        sprintf "SMap2 %O" (this.AsMap())

    override this.Equals(obj) =
        match obj with
        | :? SMap2<'Key1, 'Key2, 'Value > as s -> 
          this.AsMap() = s.AsMap()
        | _ -> false

    override this.GetHashCode () =
        hash (this.AsMap())

    member _.ContainsKey k =
        match tryFind k with
        | Some _ -> true
        | None -> false

    // Slices
    // 2D
    member this.Item
        with get (k1f, k2f) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            SMap2(keys1, keys2, this.TryFind)

    // 1D
    member this.Item
        with get (k1, k2f) =
            let keys2 = filterKeys k2f this.Keys2
            let newTryFind k = tryFind (k1, k)
            SMap (keys2, newTryFind)

    member this.Item
        with get (k1f, k2) =
            let keys1 = filterKeys k1f this.Keys1
            let newTryFind k = tryFind (k, k2)
            SMap (keys1, newTryFind)

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2) =
            match tryFind (k1, k2) with
            | Some v -> v
            | None -> raise (KeyNotFoundException("The given key was not present in the slicemap."))

    // Operators
    static member inline (*) (coef, s:SMap2<_,_,_>) =
        let newValues = TryFind.scale coef s.PossibleKeys s.TryFind
        SMap2(s.Keys1, s.Keys2, newValues)

    static member inline (*) (s:SMap2<_,_,_>, coef) =
        let newValues = TryFind.scale coef s.PossibleKeys s.TryFind
        SMap2(s.Keys1, s.Keys2, newValues)

    static member inline (.*) (lhs:SMap2<_,_,_>, rhs:SMap2<_,_,_>) =
        let keys1 = SliceSet.intersect lhs.Keys1 rhs.Keys1
        let keys2 = SliceSet.intersect lhs.Keys2 rhs.Keys2
        let keySet = seq {for k1 in keys1 do for k2 in keys2 -> (k1, k2)}
        let rKeyBuilder = id
        let newValues = TryFind.multiply keySet lhs.TryFind rKeyBuilder rhs.TryFind

        SMap2(keys1, keys2, newValues)

    static member inline (.*) (lhs:SMap2<_,_,_>, rhs:SMap<_,_>) =
        let keys1 = lhs.Keys1
        let keys2 = SliceSet.intersect lhs.Keys2 rhs.Keys
        let keySet = seq {for k1 in keys1 do for k2 in keys2 -> (k1, k2)}
        let keyBuilder = fun (x, y) -> y
        let newValues = TryFind.multiply keySet lhs.TryFind keyBuilder rhs.TryFind

        SMap2(keys1, keys2, newValues)

    static member inline (.*) (lhs:SMap<_,_>, rhs:SMap2<_,_,_>) =
        let keys1 = SliceSet.intersect lhs.Keys rhs.Keys1
        let keys2 = rhs.Keys2
        let keySet = seq {for k1 in keys1 do for k2 in keys2 -> (k1, k2)}
        let keyBuilder = fun (x, y) -> x
        let newValues = TryFind.multiply keySet rhs.TryFind keyBuilder lhs.TryFind

        SMap2(keys1, keys2, newValues)

    static member inline (+) (lhs:SMap2<_,_,_>, rhs:SMap2<_,_,_>) =
        let newKeys1 = lhs.Keys1 + rhs.Keys1
        let newKeys2 = lhs.Keys2 + rhs.Keys2
        let keySet = seq {for k1 in newKeys1 do for k2 in newKeys2 -> (k1, k2)}
        let newValues = TryFind.mergeAdd keySet lhs.TryFind rhs.TryFind
        SMap2(newKeys1, newKeys2, newValues)

    static member inline Sum (m:SMap2<_,_,_>) =
        TryFind.sum m.PossibleKeys m.TryFind

    //static member inline Sum (m:SMap2<_,_,Flips.Types.Decision>) =
    //    TryFind.sumDecisions m.PossibleKeys m.TryFind

    //static member inline Sum (m:SMap2<_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    TryFind.sumDecisionsWithUnits m.PossibleKeys m.TryFind


module SMap2 =

    let ofSeq (m:seq<_>) =
        m |> SMap2

    let toSeq (m:SMap2<_,_,_>) =
        TryFind.toSeq m.PossibleKeys m.TryFind

    let ofMap (m:Map<_,_>) =
        m |> SMap2

    let toMap (m:SMap2<_,_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> Seq.ofList |> SMap2

    let toList (m:SMap2<_,_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Seq.ofArray |> SMap2

    let toArray (m:SMap2<_,_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap2<_,_,_>) =
        m.ContainsKey k

    let inline reKey f m =
        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq