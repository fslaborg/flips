﻿namespace Flips.SliceMap

open System.Collections.Generic


type SMap3<'Key1, 'Key2, 'Key3, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Value : equality> 
    (keys1:SliceSet<'Key1>, keys2:SliceSet<'Key2>, keys3:SliceSet<'Key3>, tryFind:TryFind<('Key1 * 'Key2 * 'Key3), 'Value>) =

    let keys1 = keys1
    let keys2 = keys2
    let keys3 = keys3
    let keys = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 -> (k1, k2, k3)}
    let tryFind = tryFind

    new (s:seq<('Key1 * 'Key2 * 'Key3) * 'Value>) =
        let keys1 = s |> Seq.map (fun ((x, y, z), v) -> x) |> SliceSet
        let keys2 = s |> Seq.map (fun ((x, y, z), v) -> y) |> SliceSet
        let keys3 = s |> Seq.map (fun ((x, y, z), v) -> z) |> SliceSet
        let tryFind = TryFind.ofSeq s
        SMap3 (keys1, keys2, keys3, tryFind)

    new (m:Map<('Key1 * 'Key2 * 'Key3), 'Value>) =
      let s = m |> Map.toSeq
      SMap3 s

    interface ISliceData<('Key1 * 'Key2 * 'Key3), 'Value> with
        member _.Keys = keys
        member _.TryFind = tryFind

    member _.Keys1 = keys1
    member _.Keys2 = keys2
    member _.Keys3 = keys3
    member _.Keys = keys
    member _.TryFind = tryFind

    member _.AsMap () =
        tryFind
        |> TryFind.toMap keys

    override this.ToString () = 
        sprintf "SMap3 %O" (this.AsMap())

    override this.Equals(obj) =
        match obj with
        | :? SMap3<'Key1, 'Key2, 'Key3, 'Value> as other -> 
            let mutable result = true
            if not (Seq.equals this.Keys other.Keys) then
                result <- false

            if result then
                if not (TryFind.equals this.Keys this.TryFind other.TryFind) then
                    result <- false

            result
        | _ -> false

    override this.GetHashCode () =
        hash (this.AsMap())

    member _.ContainsKey k =
        match tryFind k with
        | Some _ -> true
        | None -> false

    // Slices
    // 3D
    member this.Item
        with get (k1f, k2f, k3f) =
            let keys1 = SliceSet.slice k1f keys1
            let keys2 = SliceSet.slice k2f keys2
            let keys3 = SliceSet.slice k3f keys3
            SMap3(keys1, keys2, keys3, tryFind)

    // 2D
    member this.Item
        with get (k1, k2f, k3f) =
            let keys2 = SliceSet.slice k2f keys2
            let keys3 = SliceSet.slice k3f keys3
            let newTryFind (k2, k3) = tryFind (k1, k2, k3)
            SMap2 (keys2, keys3, newTryFind)

    member this.Item
        with get (k1f, k2, k3f) =
            let keys1 = SliceSet.slice k1f keys1
            let keys3 = SliceSet.slice k3f keys3
            let newTryFind (k1, k3) = tryFind (k1, k2, k3)
            SMap2 (keys1, keys3, newTryFind)

    member this.Item
        with get (k1f, k2f, k3) =
            let keys1 = SliceSet.slice k1f keys1
            let keys2 = SliceSet.slice k2f keys2
            let newTryFind (k1, k2) = tryFind (k1, k2, k3)
            SMap2 (keys1, keys2, newTryFind)

    // 1D
    member this.Item
        with get (k1, k2, k3f) =
            let keys3 = SliceSet.slice k3f keys3
            let newTryFind (k3) = tryFind (k1, k2, k3)
            SMap (keys3, newTryFind)

    member this.Item
        with get (k1, k2f, k3) =
            let keys2 = SliceSet.slice k2f keys2
            let newTryFind (k2) = tryFind (k1, k2, k3)
            SMap (keys2, newTryFind)

    member this.Item
        with get (k1f, k2, k3) =
            let keys1 = SliceSet.slice k1f keys1
            let newTryFind (k1) = tryFind (k1, k2, k3)
            SMap (keys1, newTryFind)

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2, k3) =
            match tryFind (k1, k2, k3) with
            | Some v -> v
            | None -> raise (KeyNotFoundException("The given key was not present in the slicemap."))

    // Operators
    static member inline (*) (coef, s:SMap3<_,_,_,_>) =
        let newTryFind = s.TryFind >> Option.map (fun v -> coef * v)
        SMap3(s.Keys1, s.Keys2, s.Keys3, newTryFind)

    static member inline (*) (s:SMap3<_,_,_,_>, coef) =
        let newTryFind = s.TryFind >> Option.map (fun v -> coef * v)
        SMap3(s.Keys1, s.Keys2, s.Keys3, newTryFind)

    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap3<_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys3
        let newTryFind (k1, k2, k3) =
            match (a.TryFind (k1, k2, k3)), (b.TryFind (k1, k2, k3)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap3(keys1, keys2, keys3, newTryFind)

    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap2<_,_,_>) =
        let keys1 = a.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys1
        let keys3 = SliceSet.intersect a.Keys3 b.Keys2
        let newTryFind (k1, k2, k3) =
          match (a.TryFind (k1, k2, k3)), (b.TryFind (k2, k3)) with
          | Some lv, Some rv -> Some (lv * rv)
          | _,_ -> None
        SMap3(keys1, keys2, keys3, newTryFind)

    static member inline (.*) (a:SMap2<_,_,_>, b:SMap3<_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = b.Keys3
        let newTryFind (k1, k2, k3) =
            match (a.TryFind (k1, k2)), (b.TryFind (k1, k2, k3)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap3(keys1, keys2, keys3, newTryFind)

    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap<_,_>) =
        let keys1 = a.Keys1
        let keys2 = a.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys
        let newTryFind (k1, k2, k3) =
            match (a.TryFind (k1, k2, k3)), (b.TryFind (k3)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap3(keys1, keys2, keys3, newTryFind)

    static member inline (.*) (a:SMap<_,_>, b:SMap3<_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys b.Keys1
        let keys2 = b.Keys2
        let keys3 = b.Keys3
        let newTryFind (k1, k2, k3) =
            match (a.TryFind (k1)), (b.TryFind (k1, k2, k3)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap3(keys1, keys2, keys3, newTryFind)

    static member inline (+) (a:SMap3<_,_,_,_>, b:SMap3<_,_,_,_>) =
        let keys1 = a.Keys1 + b.Keys1
        let keys2 = a.Keys2 + b.Keys2
        let keys3 = a.Keys3 + b.Keys3
        let newTryFind k =
            match (a.TryFind k), (b.TryFind k) with
            | Some lv, Some rv -> Some (lv * rv)
            | Some lv, None -> Some lv
            | None, Some rv -> Some rv
            | None, None -> None
        SMap3(keys1, keys2, keys3, newTryFind)

    static member inline Sum (m:SMap3<_,_,_,_>) =
        TryFind.sum m.Keys m.TryFind


[<RequireQualifiedAccess>]
module SMap3 =

    let ofSeq m =
        m |> Map.ofSeq |> SMap3

    let toSeq (m:SMap3<_,_,_,_>) =
        TryFind.toSeq m.Keys m.TryFind

    let ofMap (m:Map<_,_>) =
        m |> SMap3

    let toMap (m:SMap3<_,_,_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> Map.ofList |> SMap3

    let toList (m:SMap3<_,_,_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Map.ofArray |> SMap3

    let toArray (m:SMap3<_,_,_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap3<_,_,_,_>) =
        m.ContainsKey k

    let inline reKey f m =
        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq