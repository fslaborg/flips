namespace Flips.SliceMap

open System
open System.Collections.Generic


type SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Value : equality> 
    (keys1:SliceSet<'Key1>, keys2:SliceSet<'Key2>, keys3:SliceSet<'Key3>, keys4:SliceSet<'Key4>, tryFind:TryFind<('Key1 * 'Key2 * 'Key3 * 'Key4), 'Value>) =

    let keys1 = keys1
    let keys2 = keys2
    let keys3 = keys3
    let keys4 = keys4
    let keys = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 do for k4 in keys4 -> (k1, k2, k3, k4)}
    let tryFind = tryFind

    let keyInRange (k1, k2, k3, k4) =
        keys1.Contains k1 && keys2.Contains k2 && keys3.Contains k3 && keys4.Contains k4

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

    interface ISliceData<('Key1 * 'Key2 * 'Key3 * 'Key4), 'Value> with
        member _.Keys = keys
        member _.TryFind = tryFind

    member _.Keys1 = keys1
    member _.Keys2 = keys2
    member _.Keys3 = keys3
    member _.Keys4 = keys4
    member _.Keys = keys
    member _.TryFind = tryFind

    member _.AsMap () =
        tryFind
        |> TryFind.toMap keys

    override this.ToString() =
        sprintf "SMap4 %O" (this.AsMap())

    override this.Equals(obj) =
        match obj with
        | :? SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> as other -> 
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
        if keyInRange k then
            match tryFind k with
            | Some _ -> true
            | None -> false
        else
            false


    member this.Item
        with get(k) =
            match (keyInRange k), (tryFind k) with
            | true, Some v -> v
            | _, _ -> raise (KeyNotFoundException("The given key was not present in the SliceMap."))

    member this.Item
        with get(k1, k2, k3, k4) =
            match (keyInRange (k1, k2, k3, k4)), (tryFind (k1, k2, k3, k4)) with
            | true, Some v -> v
            | _, _ -> raise (KeyNotFoundException("The given key was not present in the SliceMap."))

    // Slices
    // 4D
    member this.Item
        with get (k1f, k2f, k3f, k4f) =
            let keys1 = SliceSet.slice k1f keys1
            let keys2 = SliceSet.slice k2f keys2
            let keys3 = SliceSet.slice k3f keys3
            let keys4 = SliceSet.slice k4f keys4
            SMap4(keys1, keys2, keys3, keys4, tryFind)

    // 3D
    member this.Item
        with get (k1, k2f, k3f, k4f) =
            let keys2 = SliceSet.slice k2f keys2
            let keys3 = SliceSet.slice k3f keys3
            let keys4 = SliceSet.slice k4f keys4
            let newTryFind (k2, k3, k4) = tryFind (k1, k2, k3, k4)
            SMap3 (keys2, keys3, keys4, newTryFind)

    member this.Item
        with get (k1f, k2, k3f, k4f) =
            let keys1 = SliceSet.slice k1f keys1
            let keys3 = SliceSet.slice k3f keys3
            let keys4 = SliceSet.slice k4f keys4
            let newTryFind (k1, k3, k4) = tryFind (k1, k2, k3, k4)
            SMap3 (keys1, keys3, keys4, newTryFind)

    member this.Item
        with get (k1f, k2f, k3, k4f) =
            let keys1 = SliceSet.slice k1f keys1
            let keys2 = SliceSet.slice k2f keys2
            let keys4 = SliceSet.slice k4f keys4
            let newTryFind (k1, k2, k4) = tryFind (k1, k2, k3, k4)
            SMap3 (keys1, keys2, keys4, newTryFind)

    member this.Item
        with get (k1f, k2f, k3f, k4) =
            let keys1 = SliceSet.slice k1f keys1
            let keys2 = SliceSet.slice k2f keys2
            let keys3 = SliceSet.slice k3f keys3
            let newTryFind (k1, k2, k3) = tryFind (k1, k2, k3, k4)
            SMap3 (keys1, keys2, keys3, newTryFind)

    // 2D
    member this.Item
        with get (k1, k2, k3f, k4f) =
            let keys3 = SliceSet.slice k3f keys3
            let keys4 = SliceSet.slice k4f keys4
            let newTryFind (k3, k4) = tryFind (k1, k2, k3, k4)
            SMap2 (keys3, keys4, newTryFind)

    member this.Item
        with get (k1, k2f, k3, k4f) =
            let keys2 = SliceSet.slice k2f keys2
            let keys4 = SliceSet.slice k4f keys4
            let newTryFind (k2, k4) = tryFind (k1, k2, k3, k4)
            SMap2 (keys2, keys4, newTryFind)

    member this.Item
        with get (k1, k2f, k3f, k4) =
            let keys2 = SliceSet.slice k2f keys2
            let keys3 = SliceSet.slice k3f keys3
            let newTryFind (k2, k3) = tryFind (k1, k2, k3, k4)
            SMap2 (keys2, keys3, newTryFind)

    member this.Item
            with get (k1f, k2, k3, k4f) =
                let keys1 = SliceSet.slice k1f keys1
                let keys4 = SliceSet.slice k4f keys4
                let newTryFind (k1, k4) = tryFind (k1, k2, k3, k4)
                SMap2 (keys1, keys4, newTryFind)

    member this.Item
        with get (k1f, k2, k3f, k4) =
            let keys1 = SliceSet.slice k1f keys1
            let keys3 = SliceSet.slice k3f keys3
            let newTryFind (k1, k3) = tryFind (k1, k2, k3, k4)
            SMap2 (keys1, keys3, newTryFind)

    member this.Item
        with get (k1f, k2f, k3, k4) =
            let keys1 = SliceSet.slice k1f keys1
            let keys2 = SliceSet.slice k2f keys2
            let newTryFind (k1, k2) = tryFind (k1, k2, k3, k4)
            SMap2 (keys1, keys2, newTryFind)

    // 1D
    member this.Item
        with get (k1, k2, k3, k4f) =
            let keys4 = SliceSet.slice k4f keys4
            let newTryFind (k4) = tryFind (k1, k2, k3, k4)
            SMap (keys4, newTryFind)

    member this.Item
        with get (k1, k2, k3f, k4) =
            let keys3 = SliceSet.slice k3f keys3
            let newTryFind (k3) = tryFind (k1, k2, k3, k4)
            SMap (keys3, newTryFind)

    member this.Item
        with get (k1, k2f, k3, k4) =
            let keys2 = SliceSet.slice k2f keys2
            let newTryFind (k2) = tryFind (k1, k2, k3, k4)
            SMap (keys2, newTryFind)

    member this.Item
        with get (k1f, k2, k3, k4) =
            let keys1 = SliceSet.slice k1f keys1
            let newTryFind (k1) = tryFind (k1, k2, k3, k4)
            SMap (keys1, newTryFind)

    // Operators
    static member inline (*) (coef, s:SMap4<_,_,_,_,_>) =
        let newTryFind = s.TryFind >> Option.map (fun v -> coef * v)
        SMap4(s.Keys1, s.Keys2, s.Keys3, s.Keys4, newTryFind)

    static member inline (*) (s:SMap4<_,_,_,_,_>, coef) =
        let newTryFind = s.TryFind >> Option.map (fun v -> coef * v)
        SMap4(s.Keys1, s.Keys2, s.Keys3, s.Keys4, newTryFind)

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys3
        let keys4 = SliceSet.intersect a.Keys4 b.Keys4
        let newTryFind (k1, k2, k3, k4) =
            match (a.TryFind (k1, k2, k3, k4)), (b.TryFind (k1, k2, k3, k4)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap4(keys1, keys2, keys3, keys4, newTryFind)

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap3<_,_,_,_>) =
        let keys1 = a.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys1
        let keys3 = SliceSet.intersect a.Keys3 b.Keys2
        let keys4 = SliceSet.intersect a.Keys4 b.Keys3
        let newTryFind (k1, k2, k3, k4) =
            match (a.TryFind (k1, k2, k3, k4)), (b.TryFind (k2, k3, k4)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap4(keys1, keys2, keys3, keys4, newTryFind)

    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap4<_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys3
        let keys4 = b.Keys4
        let newTryFind (k1, k2, k3, k4) =
            match (a.TryFind (k1, k2, k3)), (b.TryFind (k1, k2, k3, k4)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap4(keys1, keys2, keys3, keys4, newTryFind)

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap2<_,_,_>) =
        let keys1 = a.Keys1
        let keys2 = a.Keys2
        let keys3 = SliceSet.intersect a.Keys3 b.Keys1
        let keys4 = SliceSet.intersect a.Keys4 b.Keys2
        let newTryFind (k1, k2, k3, k4) =
            match (a.TryFind (k1, k2, k3, k4)), (b.TryFind (k3, k4)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap4(keys1, keys2, keys3, keys4, newTryFind)

    static member inline (.*) (a:SMap2<_,_,_>, b:SMap4<_,_,_,_,_>) =
        let keys1 = SliceSet.intersect a.Keys1 b.Keys1
        let keys2 = SliceSet.intersect a.Keys2 b.Keys2
        let keys3 = b.Keys3
        let keys4 = b.Keys4
        let newTryFind (k1, k2, k3, k4) =
            match (a.TryFind (k1, k2)), (b.TryFind (k1, k2, k3, k4)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap4(keys1, keys2, keys3, keys4, newTryFind)

    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap<_,_>) =
        let keys1 = a.Keys1
        let keys2 = a.Keys2
        let keys3 = a.Keys3
        let keys4 = SliceSet.intersect a.Keys4 b.Keys
        let newTryFind (k1, k2, k3, k4) =
            match (a.TryFind (k1, k2, k3, k4)), (b.TryFind (k4)) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap4(keys1, keys2, keys3, keys4, newTryFind)

    static member inline (.*) (a:SMap<_,_>, b:SMap4<_,_,_,_,_>) =
            let keys1 = SliceSet.intersect a.Keys b.Keys1
            let keys2 = b.Keys2
            let keys3 = b.Keys3
            let keys4 = b.Keys4
            let newTryFind (k1, k2, k3, k4) =
                match (a.TryFind (k1)), (b.TryFind (k1, k2, k3, k4)) with
                | Some lv, Some rv -> Some (lv * rv)
                | _,_ -> None
            SMap4(keys1, keys2, keys3, keys4, newTryFind)

    static member inline (+) (a:SMap4<_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =
        let keys1 = a.Keys1 + b.Keys1
        let keys2 = a.Keys2 + b.Keys2
        let keys3 = a.Keys3 + b.Keys3
        let keys4 = a.Keys4 + b.Keys4
        let newTryFind k =
            match (a.TryFind k), (b.TryFind k) with
            | Some lv, Some rv -> Some (lv + rv)
            | Some lv, None -> Some lv
            | None, Some rv -> Some rv
            | None, None -> None
        SMap4(keys1, keys2, keys3, keys4, newTryFind)

    static member inline Sum (m:SMap4<_,_,_,_,_>) =
        TryFind.sum m.Keys m.TryFind


    interface IEnumerable<KeyValuePair<'Key1 * 'Key2 * 'Key3 * 'Key4, 'Value>> with
        member _.GetEnumerator () : IEnumerator<KeyValuePair<'Key1 * 'Key2 * 'Key3 * 'Key4, 'Value>> = 
            let s = seq { for key in keys -> tryFind key |> Option.map (fun v -> KeyValuePair(key, v)) } |> Seq.choose id
            s.GetEnumerator ()

    interface System.Collections.IEnumerable with
        member _.GetEnumerator () : Collections.IEnumerator = 
            let s = seq { for key in keys -> tryFind key |> Option.map (fun v -> KeyValuePair(key, v)) } |> Seq.choose id
            s.GetEnumerator () :> Collections.IEnumerator

    interface IDictionary<'Key1 * 'Key2 * 'Key3 * 'Key4, 'Value> with 
        member m.Item 
            with get x = m.[x] 
            and  set x v = ignore(x, v); raise (NotSupportedException("SliceMap cannot be mutated"))

        // REVIEW: this implementation could avoid copying the Values to an array 
        member m.Keys = ([| for kvp in m -> kvp.Key |] :> ICollection<'Key1 * 'Key2 * 'Key3 * 'Key4>)

        // REVIEW: this implementation could avoid copying the Values to an array 
        member m.Values = ([| for kvp in m -> kvp.Value |] :> ICollection<'Value>)

        member m.Add(k, v) = ignore(k, v); raise (NotSupportedException("SliceMap cannot be mutated"))

        member m.ContainsKey k = m.ContainsKey k

        member m.TryGetValue(k, r) = 
            match m.TryFind(k) with 
            | Some v -> 
                r <- v
                true
            | None -> false

        member m.Remove(k : 'Key1 * 'Key2 * 'Key3 * 'Key4) = ignore k; (raise (NotSupportedException("SliceMap cannot be mutated")) : bool)

    interface ICollection<KeyValuePair<'Key1 * 'Key2 * 'Key3 * 'Key4, 'Value>> with 
        member __.Add x = ignore x; raise (NotSupportedException("SliceMap cannot be mutated"))

        member __.Clear() = raise (NotSupportedException("SliceMap cannot be mutated"))

        member __.Remove x = ignore x; raise (NotSupportedException("SliceMap cannot be mutated"))

        member m.Contains x = m.ContainsKey x.Key && Unchecked.equals m.[x.Key] x.Value

        member m.CopyTo(arr, i) = raise (NotSupportedException("SliceMap does not support CopyTo"))

        member __.IsReadOnly = true

        member m.Count = raise (NotSupportedException("SliceMap does not support Count"))

    interface IReadOnlyCollection<KeyValuePair<'Key1 * 'Key2 * 'Key3 * 'Key4, 'Value>> with
        member m.Count = raise (NotSupportedException("SliceMap does not support Count"))

    interface IReadOnlyDictionary<'Key1 * 'Key2 * 'Key3 * 'Key4, 'Value> with

        member m.Item with get key = m.[key]

        member m.Keys = seq { for kvp in m -> kvp.Key }

        member m.TryGetValue(key, value: byref<'Value>) = 
            match m.TryFind(key) with 
            | Some v -> 
                value <- v
                true
            | None -> false

        member m.Values = seq { for kvp in m -> kvp.Value }

        member m.ContainsKey key = m.ContainsKey key

[<RequireQualifiedAccess>]
module SMap4 =

    let ofSeq m =
        m |> Map.ofSeq |> SMap4

    let toSeq (m:SMap4<_,_,_,_,_>) =
        TryFind.toSeq m.Keys m.TryFind

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
