namespace Flips.SliceMap

open System.Collections.Generic
open Utilities

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

    member _.Keys = keys
    member _.TryFind = tryFind

    member _.AsMap () =
      tryFind
      |> TryFind.toMap keys

    override this.ToString() =
        sprintf "SMap %O" (this.AsMap ())

    override this.Equals(obj) =
        match obj with
        | :? SMap<'Key, 'Value> as s -> 
            (this.AsMap()) = (s.AsMap())
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
            let newKeys = filterKeys k1f keys
            SMap(newKeys, tryFind)

    // 0D (aka GetItem)
    member _.Item
        with get(k) =
            match tryFind k with
            | Some v -> v
            | None -> raise (KeyNotFoundException("The given key was not present in the slicemap."))

    // Operators
    static member inline (*) (coef, smap:SMap<_,_>) =
        let newStore = TryFind.scale coef smap.Keys smap.TryFind
        SMap(smap.Keys, newStore)

    static member inline (*) (smap:SMap<_,_>, coef) =
        let newValues = TryFind.scale coef smap.Keys smap.TryFind
        SMap(smap.Keys, newValues)

    static member inline (.*) (lhs:SMap<_,_>, rhs:SMap<_,_>) =
        let newKeys = SliceSet.intersect lhs.Keys rhs.Keys
        let newValues = TryFind.multiply newKeys lhs.TryFind id rhs.TryFind
        SMap(newKeys, newValues)

    static member inline (+) (lhs:SMap<_,_>, rhs:SMap<_,_>) =
        let newKeys = lhs.Keys + rhs.Keys
        let newValues = TryFind.mergeAdd newKeys lhs.TryFind rhs.TryFind
        SMap(newKeys, newValues)

    static member inline Sum (m:SMap<_,_>) =
        TryFind.sum m.Keys m.TryFind

    //static member Sum (m:SMap<_,Flips.Types.LinearExpression>) =
    //    TryFind.sumLinearExpressions m.Keys m.TryFind

    //static member Sum (m:SMap<_,Flips.Types.Decision>) =
    //    TryFind.sumDecisions m.Keys m.TryFind

    //static member Sum (m:SMap<_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    TryFind.sumDecisionsWithUnits m.Keys m.TryFind


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
