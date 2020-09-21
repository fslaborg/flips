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
        let newTryFind k =
            smap.TryFind k
            |> Option.map (fun v -> coef * v)
        SMap(smap.Keys, newTryFind)

    static member inline (*) (smap:SMap<_,_>, coef) =
        let newTryFind k =
          smap.TryFind k
          |> Option.map (fun v -> coef * v)
        SMap(smap.Keys, newTryFind)

    static member inline (.*) (lhs:SMap<_,_>, rhs:SMap<_,_>) =
        let newKeys = SliceSet.intersect lhs.Keys rhs.Keys
        //let newTryFind = TryFind.multiply newKeys lhs.TryFind id rhs.TryFind
        let newTryFind k =
            match (lhs.TryFind k), (rhs.TryFind k) with
            | Some lv, Some rv -> Some (lv * rv)
            | _,_ -> None
        SMap(newKeys, newTryFind)

    static member inline (+) (lhs:SMap<_,_>, rhs:SMap<_,_>) =
        let newKeys = lhs.Keys + rhs.Keys
        //let newTryFind = TryFind.mergeAdd newKeys lhs.TryFind rhs.TryFind
        let newTryFind k =
          match (lhs.TryFind k), (rhs.TryFind k) with
          | Some lv, Some rv -> Some (lv * rv)
          | _,_ -> None
        SMap(newKeys, newTryFind)

    //static member Sum (m:SMap<_,float>) =
    //    TryFind.sum m.Keys m.TryFind

    //static member Sum (m:SMap<_,Flips.Types.LinearExpression>) =
    //    TryFind.sum m.Keys m.TryFind

    //static member Sum (m:SMap<_,Flips.UnitsOfMeasure.Types.LinearExpression<_>>) =
    //  TryFind.sum m.Keys m.TryFind

    static member Sum (m:SMap<_,Flips.Types.Decision>) =
        let newTryFind = m.TryFind >> Option.map (fun v -> 1.0 * v)
        TryFind.sum m.Keys newTryFind

    static member Sum (m:SMap<_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
        let newTryFind = m.TryFind >> Option.map (fun v -> 1.0 * v)
        TryFind.sum m.Keys newTryFind

[<AutoOpen>]
module SMapExtensions =

    type SMap<'Key, 'Value when 'Key : comparison and 'Value : equality> with
        static member inline Sum (m:SMap<_,_>) =
            TryFind.sum m.Keys m.TryFind


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
