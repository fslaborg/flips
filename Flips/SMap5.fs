namespace Flips.SliceMap

open System.Collections.Generic
open Utilities

type SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Key5 : comparison and 'Value : equality> 
    (keys1:SliceSet<'Key1>, keys2:SliceSet<'Key2>, keys3:SliceSet<'Key3>, keys4:SliceSet<'Key4>, keys5:SliceSet<'Key5>, tryFind:TryFind<('Key1 * 'Key2 * 'Key3 * 'Key4 * 'Key5), 'Value>) =

    let keys1 = keys1
    let keys2 = keys2
    let keys3 = keys3
    let keys4 = keys4
    let keys5 = keys5
    let possibleKeys = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
    let tryFind = tryFind

    new (s:seq<('Key1 * 'Key2 * 'Key3 * 'Key4 * 'Key5) * 'Value>) =
        let keys1 = s |> Seq.map (fun ((x, y, z, a, b), v) -> x) |> SliceSet
        let keys2 = s |> Seq.map (fun ((x, y, z, a, b), v) -> y) |> SliceSet
        let keys3 = s |> Seq.map (fun ((x, y, z, a, b), v) -> z) |> SliceSet
        let keys4 = s |> Seq.map (fun ((x, y, z, a, b), v) -> a) |> SliceSet
        let keys5 = s |> Seq.map (fun ((x, y, z, a, b), v) -> b) |> SliceSet
        let tryFind = TryFind.ofSeq s
        SMap5 (keys1, keys2, keys3, keys4, keys5, tryFind)

    new (m:Map<('Key1 * 'Key2 * 'Key3 * 'Key4 * 'Key5), 'Value>) =
      let s = m |> Map.toSeq
      SMap5 s

    member internal _.Keys1 = keys1
    member internal _.Keys2 = keys2
    member internal _.Keys3 = keys3
    member internal _.Keys4 = keys4
    member internal _.Keys5 = keys5
    member internal _.PossibleKeys = possibleKeys
    member internal _.TryFind = tryFind

    static member AsMap (sm:SMap5<_,_,_,_,_,_>) =
        sm.TryFind
        |> TryFind.toMap sm.PossibleKeys

    override this.ToString() =
        sprintf "SMap5 %O" (SMap5<_,_,_,_,_,_>.AsMap this)

    override this.Equals(obj) =
        match obj with
        | :? SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value> as s -> 
            SMap5<_,_,_,_,_,_>.AsMap this = SMap5<_,_,_,_,_,_>.AsMap s
        | _ -> false

    override this.GetHashCode () =
        hash (SMap5<_,_,_,_,_,_>.AsMap this)

    member this.ContainsKey k =
        match tryFind k with
        | Some _ -> true
        | None -> false

    // Slices
    // 5D
    member this.Item
        with get (k1f, k2f, k3f, k4f, k5f) =
        let keys1 = filterKeys k1f keys1
        let keys2 = filterKeys k2f keys2
        let keys3 = filterKeys k3f keys3
        let keys4 = filterKeys k4f keys4
        let keys5 = filterKeys k5f keys5
        SMap5(keys1, keys2, keys3, keys4, keys5, tryFind)

    // 4D
    member this.Item
        with get (k1, k2f, k3f, k4f, k5f) =
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let keys5 = filterKeys k5f keys5
            let newTryFind (k2, k3, k4, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap4 (keys2, keys3, keys4, keys5, newTryFind)

    member this.Item
        with get (k1f, k2, k3f, k4f, k5f) =
            let keys1 = filterKeys k1f keys1
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let keys5 = filterKeys k5f keys5
            let newTryFind (k1, k3, k4, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap4 (keys1, keys3, keys4, keys5, newTryFind)

    member this.Item
        with get (k1f, k2f, k3, k4f, k5f) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys4 = filterKeys k4f keys4
            let keys5 = filterKeys k5f keys5
            let newTryFind (k1, k2, k4, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap4 (keys1, keys2, keys4, keys5, newTryFind)

    member this.Item
        with get (k1f, k2f, k3f, k4, k5f) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let keys5 = filterKeys k5f keys5
            let newTryFind (k1, k2, k3, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap4 (keys1, keys2, keys3, keys5, newTryFind)

    member this.Item
        with get (k1f, k2f, k3f, k4f, k5) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let newTryFind (k1, k2, k3, k4) = tryFind (k1, k2, k3, k4, k5)
            SMap4 (keys1, keys2, keys3, keys4, newTryFind)


    // 3D
    member this.Item
        with get (k1, k2, k3f, k4f, k5f) =
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let keys5 = filterKeys k5f keys5
            let newTryFind (k3, k4, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys3, keys4, keys5, newTryFind)

    member this.Item
        with get (k1, k2f, k3, k4f, k5f) =
            let keys2 = filterKeys k2f keys2
            let keys4 = filterKeys k4f keys4
            let keys5 = filterKeys k5f keys5
            let newTryFind (k2, k4, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys2, keys4, keys5, newTryFind)

    member this.Item
        with get (k1, k2f, k3f, k4, k5f) =
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let keys5 = filterKeys k5f keys5
            let newTryFind (k2, k3, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys2, keys3, keys5, newTryFind)

    member this.Item
        with get (k1, k2f, k3f, k4f, k5) =
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let newTryFind (k2, k3, k4) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys2, keys3, keys4, newTryFind)

    member this.Item
        with get (k1f, k2, k3, k4f, k5f) =
            let keys1 = filterKeys k1f keys1
            let keys4 = filterKeys k4f keys4
            let keys5 = filterKeys k5f keys5
            let newTryFind (k1, k4, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys1, keys4, keys5, newTryFind)

    member this.Item
        with get (k1f, k2, k3f, k4, k5f) =
            let keys1 = filterKeys k1f keys1
            let keys3 = filterKeys k3f keys3
            let keys5 = filterKeys k5f keys5
            let newTryFind (k1, k3, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys1, keys3, keys5, newTryFind)

    member this.Item
        with get (k1f, k2, k3f, k4f, k5) =
            let keys1 = filterKeys k1f keys1
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let newTryFind (k1, k3, k4) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys1, keys3, keys4, newTryFind)

    member this.Item
        with get (k1f, k2f, k3, k4, k5f) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys5 = filterKeys k5f keys5
            let newTryFind (k1, k2, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys1, keys2, keys5, newTryFind)

    member this.Item
        with get (k1f, k2f, k3, k4f, k5) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys4 = filterKeys k4f keys4
            let newTryFind (k1, k2, k4) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys1, keys2, keys4, newTryFind)

    member this.Item
        with get (k1f, k2f, k3f, k4, k5) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let newTryFind (k1, k2, k3) = tryFind (k1, k2, k3, k4, k5)
            SMap3 (keys1, keys2, keys3, newTryFind)


    // 2D
    member this.Item
        with get (k1, k2, k3, k4f, k5f) =
            let keys4 = filterKeys k4f keys4
            let keys5 = filterKeys k5f keys5
            let newTryFind (k4, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap2 (keys4, keys5, newTryFind)

    member this.Item
        with get (k1, k2, k3f, k4, k5f) =
            let keys3 = filterKeys k3f keys3
            let keys5 = filterKeys k5f keys5
            let newTryFind (k3, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap2 (keys3, keys5, newTryFind)

    member this.Item
        with get (k1, k2, k3f, k4f, k5) =
            let keys3 = filterKeys k3f keys3
            let keys4 = filterKeys k4f keys4
            let newTryFind (k3, k4) = tryFind (k1, k2, k3, k4, k5)
            SMap2 (keys3, keys4, newTryFind)

    member this.Item
        with get (k1f, k2, k3, k4, k5f) =
            let keys1 = filterKeys k1f keys1
            let keys5 = filterKeys k5f keys5
            let newTryFind (k1, k5) = tryFind (k1, k2, k3, k4, k5)
            SMap2 (keys1, keys5, newTryFind)

    member this.Item
        with get (k1f, k2, k3, k4f, k5) =
            let keys1 = filterKeys k1f keys1
            let keys4 = filterKeys k4f keys4
            let newTryFind (k1, k4) = tryFind (k1, k2, k3, k4, k5)
            SMap2 (keys1, keys4, newTryFind)

    member this.Item
        with get (k1f, k2f, k3, k4, k5) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let newTryFind (k1, k2) = tryFind (k1, k2, k3, k4, k5)
            SMap2 (keys1, keys2, newTryFind)

    // 1D
    member this.Item
        with get (k1, k2, k3, k4, k5f) =
            let keys5 = filterKeys k5f keys5
            let newTryFind (k5) = tryFind (k1, k2, k3, k4, k5)
            SMap (keys5, newTryFind)

    member this.Item
        with get (k1, k2, k3, k4f, k5) =
            let keys4 = filterKeys k4f keys4
            let newTryFind (k4) = tryFind (k1, k2, k3, k4, k5)
            SMap (keys4, newTryFind)

    member this.Item
        with get (k1, k2, k3f, k4, k5) =
            let keys3 = filterKeys k3f keys3
            let newTryFind (k3) = tryFind (k1, k2, k3, k4, k5)
            SMap (keys3, newTryFind)

    member this.Item
        with get (k1, k2f, k3, k4, k5) =
            let keys2 = filterKeys k2f keys2
            let newTryFind (k2) = tryFind (k1, k2, k3, k4, k5)
            SMap (keys2, newTryFind)

    member this.Item
        with get (k1f, k2, k3, k4, k5) =
            let keys1 = filterKeys k1f keys1
            let newTryFind (k1) = tryFind (k1, k2, k3, k4, k5)
            SMap (keys1, newTryFind)

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2, k3, k4, k5) =
            match tryFind (k1, k2, k3, k4, k5) with
            | Some v -> v
            | None -> raise (KeyNotFoundException("The given key was not present in the slicemap."))

    // Operators
    static member inline (*) (coef, s:SMap5<_,_,_,_,_,_>) =
        let newValues = TryFind.scale coef s.PossibleKeys s.TryFind
        SMap5(s.Keys1, s.Keys2, s.Keys3, s.Keys4, s.Keys5, newValues)

    static member inline (*) (s:SMap5<_,_,_,_,_,_>, coef) =
        let newValues = TryFind.scale coef s.PossibleKeys s.TryFind
        SMap5(s.Keys1, s.Keys2, s.Keys3, s.Keys4, s.Keys5, newValues)

    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap5<_,_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys3
        let keys4 = SliceSet.intersect a.Keys4 b.Keys4
        let keys5 = SliceSet.intersect a.Keys5 b.Keys5
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let rKeyBuilder = id
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)

    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =
        let keys1 = a.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys1
        let keys3 = SliceSet.intersect a.Keys3 b.Keys2
        let keys4 = SliceSet.intersect a.Keys4 b.Keys3
        let keys5 = SliceSet.intersect a.Keys1 b.Keys4
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let rKeyBuilder = fun (k1, k2, k3, k4, k5) -> (k2, k3, k4, k5)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)

    static member inline (.*) (b:SMap4<_,_,_,_,_>, a:SMap5<_,_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys3
        let keys4 = SliceSet.intersect a.Keys4 b.Keys4
        let keys5 = a.Keys5
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let rKeyBuilder = fun (k1, k2, k3, k4, k5) -> (k1, k2, k3, k4)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)

    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap3<_,_,_,_>) =
        let keys1 = a.Keys1
        let keys2 = a.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys1
        let keys4 = SliceSet.intersect a.Keys4 b.Keys2
        let keys5 = SliceSet.intersect a.Keys1 b.Keys3
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let rKeyBuilder = fun (k1, k2, k3, k4, k5) -> (k3, k4, k5)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)

    static member inline (.*) (b:SMap3<_,_,_,_>, a:SMap5<_,_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys3
        let keys4 = a.Keys4
        let keys5 = a.Keys5
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let rKeyBuilder = fun (k1, k2, k3, k4, k5) -> (k1, k2, k3)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)

    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap2<_,_,_>) =
        let keys1 = a.Keys1
        let keys2 = a.Keys2
        let keys3 = a.Keys3
        let keys4 = SliceSet.intersect a.Keys4 b.Keys1
        let keys5 = SliceSet.intersect a.Keys1 b.Keys2
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let rKeyBuilder = fun (k1, k2, k3, k4, k5) -> (k4, k5)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)

    static member inline (.*) (b:SMap2<_,_,_>, a:SMap5<_,_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = a.Keys3
        let keys4 = a.Keys4
        let keys5 = a.Keys5
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let rKeyBuilder = fun (k1, k2, k3, k4, k5) -> (k1, k2)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)

    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap<_,_>) =
        let keys1 = a.Keys1
        let keys2 = a.Keys2
        let keys3 = a.Keys3
        let keys4 = a.Keys4
        let keys5 = SliceSet.intersect a.Keys1 b.Keys
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let rKeyBuilder = fun (k1, k2, k3, k4, k5) -> (k5)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)

    static member inline (.*) (b:SMap<_,_>, a:SMap5<_,_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys
        let keys2 = a.Keys2
        let keys3 = a.Keys3
        let keys4 = a.Keys4
        let keys5 = a.Keys5
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let rKeyBuilder = fun (k1, k2, k3, k4, k5) -> (k1)
        let newValues = TryFind.multiply keySet a.TryFind rKeyBuilder b.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)


    static member inline (+) (lhs:SMap5<_,_,_,_,_,_>, rhs:SMap5<_,_,_,_,_,_>) =
        let keys1 = lhs.Keys1 + rhs.Keys1
        let keys2 = lhs.Keys2 + rhs.Keys2
        let keys3 = lhs.Keys3 + rhs.Keys3
        let keys4 = lhs.Keys4 + rhs.Keys4
        let keys5 = lhs.Keys5 + rhs.Keys5
        let keySet = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 do for k5 in keys5 -> (k1, k2, k3, k4, k5)}
        let newValues = TryFind.mergeAdd keySet lhs.TryFind rhs.TryFind
        SMap5(keys1, keys2, keys3, keys4, keys5, newValues)

    static member inline Sum (m:SMap5<_,_,_,_,_,_>) =
        TryFind.sum m.PossibleKeys m.TryFind

    //static member inline Sum (m:SMap5<_,_,_,_,_,Flips.Types.Decision>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap5<_,_,_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


module SMap5 =

    let ofSeq m =
        m |> Map.ofSeq |> SMap5

    let toSeq (m:SMap5<_,_,_,_,_,_>) =
        TryFind.toSeq m.PossibleKeys m.TryFind

    let ofMap (m:Map<_,_>) =
        m |> SMap5

    let toMap (m:SMap5<_,_,_,_,_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> Map.ofList |> SMap5

    let toList (m:SMap5<_,_,_,_,_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Map.ofArray |> SMap5

    let toArray (m:SMap5<_,_,_,_,_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap5<_,_,_,_,_,_>) =
        m.ContainsKey k

    let reKey f m =
        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq