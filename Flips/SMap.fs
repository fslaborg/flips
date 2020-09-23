namespace Flips.SliceMap

open System.Collections.Generic

[<NoComparison>]
type SMap<'Key, 'Value when 'Key : comparison and 'Value : equality> 
    (keys:SliceSet<'Key>, tryFind:TryFind<'Key, 'Value>) =

    let keys = keys
    let tryFind = tryFind

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
        match tryFind k with
        | Some _ -> true
        | None -> false

    // Slices
    // 1D
    member _.Item
        with get (k1f) =
            let newKeys = SliceSet.slice k1f keys
            SMap(newKeys, tryFind)

    // 0D (aka GetItem)
    member _.Item
        with get(k) =
            match tryFind k with
            | Some v -> v
            | None -> raise (KeyNotFoundException("The given key was not present in the slicemap."))

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
            | Some lv, Some rv -> Some (lv * rv)
            | Some lv, None -> Some lv
            | None, Some rv -> Some rv
            | None, None -> None
        SMap(newKeys, newTryFind)

    static member inline Sum (m:SMap<_, _>) =
        TryFind.sum m.Keys m.TryFind


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
