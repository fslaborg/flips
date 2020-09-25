﻿namespace Flips.SliceMap

open System.Collections.Generic


type SMap2<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Value : equality> 
    (keys1:SliceSet<'Key1>, keys2:SliceSet<'Key2>, tryFind:TryFind<('Key1 * 'Key2), 'Value>) =

    let keys1 = keys1
    let keys2 = keys2
    let keys = seq {for k1 in keys1 do for k2 in keys2 -> (k1, k2)}
    let tryFind = tryFind

    new (s:seq<('Key1 * 'Key2) * 'Value>) =
        let keys1 = s |> Seq.map (fst >> fst) |> SliceSet
        let keys2 = s |> Seq.map (fst >> snd) |> SliceSet
        let store = TryFind.ofSeq s
        SMap2 (keys1, keys2, store)

    new (m:Map<('Key1 * 'Key2), 'Value>) =
      let s = m |> Map.toSeq
      SMap2 s

    interface ISliceData<('Key1 * 'Key2), 'Value> with
      member _.Keys = keys
      member _.TryFind = tryFind


    member _.Keys1 = keys1
    member _.Keys2 = keys2
    member _.Keys = keys
    member _.TryFind = tryFind

    member _.AsMap () =
        tryFind
        |> TryFind.toMap keys

    override this.ToString () = 
        sprintf "SMap2 %O" (this.AsMap())

    override this.Equals(obj) =
        match obj with
        | :? SMap2<'Key1, 'Key2, 'Value> as other -> 
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

    member _.ContainsKey (k1, k2) =
        if keys1.Contains k1 && keys2.Contains k2 then
            match tryFind (k1, k2) with
            | Some _ -> true
            | None -> false
        else
            false

    // Slices
    // 2D
    member this.Item
        with get (k1f, k2f) =
            let keys1 = SliceSet.slice k1f keys1
            let keys2 = SliceSet.slice k2f keys2
            SMap2(keys1, keys2, this.TryFind)

    // 1D
    member this.Item
        with get (k1, k2f) =
            let keys2 = SliceSet.slice k2f this.Keys2
            let newTryFind k = tryFind (k1, k)
            SMap (keys2, newTryFind)

    member this.Item
        with get (k1f, k2) =
            let keys1 = SliceSet.slice k1f this.Keys1
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
        let newTryFind = s.TryFind >> Option.map (fun v -> coef * v)
        SMap2(s.Keys1, s.Keys2, newTryFind)

    static member inline (*) (s:SMap2<_,_,_>, coef) =
        let newTryFind = s.TryFind >> Option.map (fun v -> coef * v)
        SMap2(s.Keys1, s.Keys2, newTryFind)

    static member inline (.*) (a:SMap2<_,_,_>, b:SMap2<_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let newTryFind (k1, k2) =
            match (a.TryFind (k1, k2)), (b.TryFind (k1, k2)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap2(keys1, keys2, newTryFind)

    static member inline (.*) (a:SMap2<_,_,_>, b:SMap<_,_>) =
        let keys1 = a.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys
        let newTryFind (k1, k2) =
          match (a.TryFind (k1, k2)), (b.TryFind (k2)) with
          | Some lv, Some rv -> Some (lv * rv)
          | _,_ -> None
        SMap2(keys1, keys2, newTryFind)

    static member inline (.*) (a:SMap<_,_>, b:SMap2<_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys b.Keys1
        let keys2 = b.Keys2
        let newTryFind (k1, k2) =
            match (a.TryFind (k1)), (b.TryFind (k1, k2)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap2(keys1, keys2, newTryFind)

    static member inline (+) (a:SMap2<_,_,_>, b:SMap2<_,_,_>) =
        let newKeys1 = a.Keys1 + b.Keys1
        let newKeys2 = a.Keys2 + b.Keys2
        let newTryFind k =
            match (a.TryFind k), (b.TryFind k) with
            | Some lv, Some rv -> Some (lv * rv)
            | Some lv, None -> Some lv
            | None, Some rv -> Some rv
            | None, None -> None
        SMap2(newKeys1, newKeys2, newTryFind)

    static member inline Sum (m:SMap2<_,_,_>) =
        TryFind.sum m.Keys m.TryFind


[<RequireQualifiedAccess>]
module SMap2 =

    let ofSeq (m:seq<_>) =
        m |> SMap2

    let toSeq (m:SMap2<_,_,_>) =
        TryFind.toSeq m.Keys m.TryFind

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