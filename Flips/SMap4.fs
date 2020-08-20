namespace Flips.SliceMap

open System.Collections.Generic
open Utilities

type SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Value : equality> 
    (keys1:SliceSet<'Key1>, keys2:SliceSet<'Key2>, keys3:SliceSet<'Key3>, keys4:SliceSet<'Key4>, tryFind:TryFind<('Key1 * 'Key2 * 'Key3 * 'Key4), 'Value>) =

    let keys1 = keys1
    let keys2 = keys2
    let keys3 = keys3
    let keys4 = keys4
    let possibleKeys = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
    let tryFind = tryFind

    new (s:seq<('Key1 * 'Key2 * 'Key3 * 'Key4) * 'Value>) =
        let keys1 = s |> Seq.map (fun ((x, y, z, a), v) -> x) |> SliceSet
        let keys2 = s |> Seq.map (fun ((x, y, z, a), v) -> y) |> SliceSet
        let keys3 = s |> Seq.map (fun ((x, y, z, a), v) -> z) |> SliceSet
        let keys4 = s |> Seq.map (fun ((x, y, z, a), v) -> a) |> SliceSet
        let tryFind = TryFind.ofSeq s
        SMap4 (keys1, keys2, keys3, keys4, tryFind)

    new (m:Map<('Key1 * 'Key2 * 'Key3 * 'Key4), 'Value>) =
      let s = m |> Map.toSeq
      SMap4 s

    member _.Keys1 = keys1
    member _.Keys2 = keys2
    member _.Keys3 = keys3
    member _.Keys4 = keys4
    member _.PossibleKeys = possibleKeys
    member _.TryFind = tryFind

    member _.AsMap () =
        tryFind
        |> TryFind.toMap possibleKeys

    override this.ToString() =
        sprintf "SMap4 %O" (SMap4<_,_,_,_,_>.AsMap this)

    override this.Equals(obj) =
        match obj with
        | :? SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> as s -> 
            SMap4<_,_,_,_,_>.AsMap this = SMap4<_,_,_,_,_>.AsMap s
        | _ -> false

    override this.GetHashCode () =
        hash (SMap4<_,_,_,_,_>.AsMap this)

    member this.ContainsKey k =
        match tryFind k with
        | Some _ -> true
        | None -> false

    // Slices
    // 4D
    member this.Item
        with get (k1f, k2f, k3f, k4f) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            SMap4(keys1, keys2, keys3, keys4, tryFind)

    // 3D
    member this.Item
        with get (k1, k2f, k3f, k4f) =
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let newTryFind (k2, k3, k4) = tryFind (k1, k2, k3, k4)
            SMap3 (keys2, keys3, keys4, newTryFind)

    member this.Item
        with get (k1f, k2, k3f, k4f) =
            let keys1 = filterKeys k1f keys1
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let newTryFind (k1, k3, k4) = tryFind (k1, k2, k3, k4)
            SMap3 (keys1, keys3, keys4, newTryFind)

    member this.Item
        with get (k1f, k2f, k3, k4f) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys4 = filterKeys k4f keys4
            let newTryFind (k1, k2, k4) = tryFind (k1, k2, k3, k4)
            SMap3 (keys1, keys2, keys4, newTryFind)

    member this.Item
        with get (k1f, k2f, k3f, k4) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let newTryFind (k1, k2, k3) = tryFind (k1, k2, k3, k4)
            SMap3 (keys1, keys2, keys3, newTryFind)

    // 2D
    member this.Item
        with get (k1, k2, k3f, k4f) =
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let newTryFind (k3, k4) = tryFind (k1, k2, k3, k4)
            SMap2 (keys3, keys4, newTryFind)

    member this.Item
        with get (k1, k2f, k3, k4f) =
            let keys2 = filterKeys k2f keys2
            let keys4 = filterKeys k4f keys4
            let newTryFind (k2, k4) = tryFind (k1, k2, k3, k4)
            SMap2 (keys2, keys4, newTryFind)

    member this.Item
        with get (k1, k2f, k3f, k4) =
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let newTryFind (k2, k3) = tryFind (k1, k2, k3, k4)
            SMap2 (keys2, keys3, newTryFind)

    member this.Item
        with get (k1f, k2, k3f, k4) =
            let keys1 = filterKeys k1f keys1
            let keys3 = filterKeys k3f keys3
            let newTryFind (k1, k3) = tryFind (k1, k2, k3, k4)
            SMap2 (keys1, keys3, newTryFind)

    member this.Item
        with get (k1f, k2f, k3, k4) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let newTryFind (k1, k2) = tryFind (k1, k2, k3, k4)
            SMap2 (keys1, keys2, newTryFind)

    // 1D
    member this.Item
        with get (k1, k2, k3, k4f) =
            let keys4 = filterKeys k4f keys4
            let newTryFind (k4) = tryFind (k1, k2, k3, k4)
            SMap (keys4, newTryFind)

    member this.Item
        with get (k1, k2, k3f, k4) =
            let keys3 = filterKeys k3f keys3
            let newTryFind (k3) = tryFind (k1, k2, k3, k4)
            SMap (keys3, newTryFind)

    member this.Item
        with get (k1, k2f, k3, k4) =
            let keys2 = filterKeys k2f keys2
            let newTryFind (k2) = tryFind (k1, k2, k3, k4)
            SMap (keys2, newTryFind)

    member this.Item
        with get (k1f, k2, k3, k4) =
            let keys1 = filterKeys k1f keys1
            let newTryFind (k1) = tryFind (k1, k2, k3, k4)
            SMap (keys1, newTryFind)

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2, k3, k4) =
            match tryFind (k1, k2, k3, k4) with
            | Some v -> v
            | None -> raise (KeyNotFoundException("The given key was not present in the slicemap."))

    // Operators
    static member inline (*) (coef, s:SMap4<_,_,_,_,_>) =
        let newValues = TryFind.scale coef s.PossibleKeys s.TryFind
        SMap4(s.Keys1, s.Keys2, s.Keys3, s.Keys4, newValues)

    static member inline (*) (s:SMap4<_,_,_,_,_>, coef) =
        let newValues = TryFind.scale coef s.PossibleKeys s.TryFind
        SMap4(s.Keys1, s.Keys2, s.Keys3, s.Keys4, newValues)

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys3
        let keys4 = SliceSet.intersect a.Keys4 b.Keys4
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
        let rKeyBuilder = id
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap4(keys1, keys2, keys3, keys4, newValues)

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap3<_,_,_,_>) =
        let keys1 = a.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys1
        let keys3 = SliceSet.intersect a.Keys3 b.Keys2
        let keys4 = SliceSet.intersect a.Keys4 b.Keys3
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
        let rKeyBuilder = fun (k1, k2, k3, k4) -> (k2, k3, k4)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap4(keys1, keys2, keys3, keys4, newValues)

    static member inline (.*) (b:SMap3<_,_,_,_>, a:SMap4<_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys3
        let keys4 = a.Keys4
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
        let rKeyBuilder = fun (k1, k2, k3, k4) -> (k1, k2, k3)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap4(keys1, keys2, keys3, keys4, newValues)

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap2<_,_,_>) =
        let keys1 = a.Keys1
        let keys2 = a.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys1
        let keys4 = SliceSet.intersect a.Keys4 b.Keys2
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
        let rKeyBuilder = fun (k1, k2, k3, k4) -> (k3, k4)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap4(keys1, keys2, keys3, keys4, newValues)

    static member inline (.*) (b:SMap2<_,_,_>, a:SMap4<_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = a.Keys3
        let keys4 = a.Keys4
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
        let rKeyBuilder = fun (k1, k2, k3, k4) -> (k1, k2)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap4(keys1, keys2, keys3, keys4, newValues)

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap<_,_>) =
        let keys1 = a.Keys1
        let keys2 = a.Keys2
        let keys3 = a.Keys3
        let keys4 = SliceSet.intersect a.Keys4 b.Keys
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
        let rKeyBuilder = fun (k1, k2, k3, k4) -> (k4)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap4(keys1, keys2, keys3, keys4, newValues)

    static member inline (.*) (b:SMap<_,_>, a:SMap4<_,_,_,_,_>) =
            let keys1 = SliceSet.intersect a.Keys1 b.Keys
            let keys2 = a.Keys2
            let keys3 = a.Keys3
            let keys4 = a.Keys4
            let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
            let rKeyBuilder = fun (k1, k2, k3, k4) -> (k1)
            let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
            SMap4(keys1, keys2, keys3, keys4, newValues)

    static member inline (+) (lhs:SMap4<_,_,_,_,_>, rhs:SMap4<_,_,_,_,_>) =
        let keys1 = lhs.Keys1 + rhs.Keys1
        let keys2 = lhs.Keys2 + rhs.Keys2
        let keys3 = lhs.Keys3 + rhs.Keys3
        let keys4 = lhs.Keys4 + rhs.Keys4
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
        let newValues = TryFind.mergeAdd keySet lhs.TryFind rhs.TryFind
        SMap4(keys1, keys2, keys3, keys4, newValues)

    static member inline Sum (m:SMap4<_,_,_,_,_>) =
        TryFind.sum m.PossibleKeys m.TryFind

//    static member inline Sum (m:SMap4<_,_,_,_,Flips.Types.Decision>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap4<_,_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap4 =

    let ofSeq m =
        m |> Map.ofSeq |> SMap4

    let toSeq (m:SMap4<_,_,_,_,_>) =
        TryFind.toSeq m.PossibleKeys m.TryFind

    let ofMap (m:Map<_,_>) =
        m |> SMap4

    let toMap (m:SMap4<_,_,_,_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> Map.ofList |> SMap4

    let toList (m:SMap4<_,_,_,_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Map.ofArray |> SMap4

    let toArray (m:SMap4<_,_,_,_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap4<_,_,_,_,_>) =
        m.ContainsKey k

    let reKey f m =
        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq
