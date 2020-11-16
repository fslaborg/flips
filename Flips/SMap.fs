namespace Flips.SliceMap

open System
open System.Collections.Generic

[<NoComparison>]
type SMap<'Key, 'Value when 'Key : comparison and 'Value : equality> 
    (keys:SliceSet<'Key>, tryFind:TryFind<'Key, 'Value>) =

    let keys = keys
    let tryFind = tryFind

    let keyInRange k =
        keys.Contains k

    new (s:seq<'Key * 'Value>) =
        let keys = s |> Seq.map fst |> SliceSet
        let store = TryFind.ofSeq s
        SMap (keys, store)

    new (m:Map<'Key, 'Value>) =
      let s = m |> Map.toSeq
      SMap s

    interface ISliceData<'Key, 'Value> with
        member _.Keys = SliceSet.toSeq keys
        member _.TryFind = tryFind

    member _.Keys = keys
    member _.TryFind = tryFind

    member _.AsMap () =
      tryFind
      |> TryFind.toMap keys

    override this.ToString() =
        sprintf "SMap %O" (this.AsMap ())

    override this.Equals(obj) =
        match obj with
        | :? SMap<'Key, 'Value> as other -> 
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

    // Slices
    // 1D
    member _.Item
        with get (k1f) =
            let newKeys = SliceSet.slice k1f keys
            SMap(newKeys, tryFind)

    // Operators
    static member inline (*) (coef, smap:SMap<_,_>) =
        let newTryFind k =
            smap.TryFind k
            |> Option.map (fun v -> coef * v)
        SMap(smap.Keys, newTryFind)

    static member inline (*) (smap:SMap<_,_>, coef) =
        let newTryFind k =
          smap.TryFind k
          |> Option.map (fun v -> coef * v)
        SMap(smap.Keys, newTryFind)

    static member inline (.*) (a:SMap<_,_>, b:SMap<_,_>) =
        let newKeys = a.Keys + b.Keys
        let newTryFind k =
            match (a.TryFind k), (b.TryFind k) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap(newKeys, newTryFind)

    static member inline (+) (a:SMap<_,_>, b:SMap<_,_>) =
        let newKeys = SliceSet.union a.Keys b.Keys
        let newTryFind k =
            match (a.TryFind k), (b.TryFind k) with
            | Some lv, Some rv -> Some (lv + rv)
            | Some lv, None -> Some lv
            | None, Some rv -> Some rv
            | None, None -> None
        SMap(newKeys, newTryFind)

    static member inline Sum (m:SMap<_, _>) =
        TryFind.sum m.Keys m.TryFind

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member _.GetEnumerator () : IEnumerator<KeyValuePair<'Key, 'Value>> = 
            let s = seq { for key in keys -> tryFind key |> Option.map (fun v -> KeyValuePair(key, v)) } |> Seq.choose id
            s.GetEnumerator ()

    interface System.Collections.IEnumerable with
        member _.GetEnumerator () : Collections.IEnumerator = 
            let s = seq { for key in keys -> tryFind key |> Option.map (fun v -> KeyValuePair(key, v)) } |> Seq.choose id
            s.GetEnumerator () :> Collections.IEnumerator

    interface IDictionary<'Key, 'Value> with 
        member m.Item 
            with get x = m.[x] 
            and  set x v = ignore(x, v); raise (NotSupportedException("SliceMap cannot be mutated"))

        // REVIEW: this implementation could avoid copying the Values to an array 
        member m.Keys = ([| for kvp in m -> kvp.Key |] :> ICollection<'Key>)

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

        member m.Remove(k : 'Key) = ignore k; (raise (NotSupportedException("SliceMap cannot be mutated")) : bool)

    interface ICollection<KeyValuePair<'Key, 'Value>> with 
        member __.Add x = ignore x; raise (NotSupportedException("SliceMap cannot be mutated"))

        member __.Clear() = raise (NotSupportedException("SliceMap cannot be mutated"))

        member __.Remove x = ignore x; raise (NotSupportedException("SliceMap cannot be mutated"))

        member m.Contains x = m.ContainsKey x.Key && Unchecked.equals m.[x.Key] x.Value

        member m.CopyTo(arr, i) = raise (NotSupportedException("SliceMap does not support CopyTo"))

        member __.IsReadOnly = true

        member m.Count = raise (NotSupportedException("SliceMap does not support Count"))

    interface IReadOnlyCollection<KeyValuePair<'Key, 'Value>> with
        member m.Count = raise (NotSupportedException("SliceMap does not support Count"))

    interface IReadOnlyDictionary<'Key, 'Value> with

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
module SMap =

    let ofSeq (m:seq<_>) =
        m |> SMap

    let toSeq (m:SMap<_,_>) =
        TryFind.toSeq m.Keys m.TryFind

    let ofMap (m:Map<_,_>) =
        m |> Map.toSeq |> ofSeq

    let toMap (m:SMap<_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> Seq.ofList |> SMap

    let toList (m:SMap<_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Seq.ofArray |> SMap

    let toArray (m:SMap<_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap<_,_>) =
        m.ContainsKey k
