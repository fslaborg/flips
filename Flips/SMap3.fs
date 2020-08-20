namespace Flips.SliceMap

open System.Collections.Generic
open Utilities

type SMap3<'Key1, 'Key2, 'Key3, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Value : equality> 
    internal (keys1:SliceSet<'Key1>, keys2:SliceSet<'Key2>, keys3:SliceSet<'Key3>, tryFind:TryFind<('Key1 * 'Key2 * 'Key3), 'Value>) =

    let keys1 = keys1
    let keys2 = keys2
    let keys3 = keys3
    let possibleKeys = seq {for k1 in keys1 do for k2 in keys2 do for k3 in keys3 -> (k1, k2, k3)}
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

    member internal _.Keys1 = keys1
    member internal _.Keys2 = keys2
    member internal _.Keys3 = keys3
    member internal _.PossibleKeys = possibleKeys
    member internal _.TryFind = tryFind

    static member AsMap (sm:SMap3<_,_,_,_>) =
        sm.TryFind
        |> TryFind.toMap sm.PossibleKeys

    override this.ToString () = 
        sprintf "SMap3 %O" (SMap3<_,_,_,_>.AsMap this)

    override this.Equals(obj) =
        match obj with
        | :? SMap3<'Key1, 'Key2, 'Key3, 'Value > as s -> 
          SMap3<_,_,_,_>.AsMap this = SMap3<_,_,_,_>.AsMap s
        | _ -> false

    override this.GetHashCode () =
        hash (SMap3<_,_,_,_>.AsMap this)

    // Slices
    // 3D
    member this.Item
        with get (k1f, k2f, k3f) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            SMap3(keys1, keys2, keys3, tryFind)

    // 2D
    member this.Item
        with get (k1, k2f, k3f) =
            let keys2 = filterKeys k2f keys2
            let keys3 = filterKeys k3f keys3
            let newTryFind (k2, k3) = tryFind (k1, k2, k3)
            SMap2 (keys2, keys3, newTryFind)

    member this.Item
        with get (k1f, k2, k3f) =
            let keys1 = filterKeys k1f keys1
            let keys3 = filterKeys k3f keys3
            let newTryFind (k1, k3) = tryFind (k1, k2, k3)
            SMap2 (keys1, keys3, newTryFind)

    member this.Item
        with get (k1f, k2f, k3) =
            let keys1 = filterKeys k1f keys1
            let keys2 = filterKeys k2f keys2
            let newTryFind (k1, k2) = tryFind (k1, k2, k3)
            SMap2 (keys1, keys2, newTryFind)

    // 1D
    member this.Item
        with get (k1, k2, k3f) =
            let keys3 = filterKeys k3f keys3
            let newTryFind (k3) = tryFind (k1, k2, k3)
            SMap (keys3, newTryFind)

    member this.Item
        with get (k1, k2f, k3) =
            let keys2 = filterKeys k2f keys2
            let newTryFind (k2) = tryFind (k1, k2, k3)
            SMap (keys2, newTryFind)

    member this.Item
        with get (k1f, k2, k3) =
            let keys1 = filterKeys k1f keys1
            let newTryFind (k1) = tryFind (k1, k2, k3)
            SMap (keys1, newTryFind)

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2, k3) =
            match tryFind (k1, k2, k3) with
            | Some v -> v
            | None -> raise (KeyNotFoundException("The given key was not present in the slicemap."))

    // Operators
    static member inline (*) (lhs, rhs:SMap3<_,_,_,_>) =
        rhs.Values
        |> Map.map (fun k v -> lhs * v)
        |> SMap3

    static member inline (*) (lhs:SMap3<_,_,_,_>, rhs) =
        lhs.Values
        |> Map.map (fun k v -> rhs * v)
        |> SMap3

    static member inline (.*) (lhs:SMap3<_,_,_,_>, rhs:SMap3<_,_,_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun (k1, k2, k3) v -> v * rhs.[k1, k2, k3])
        |> SMap3

    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap2<_,_,_>) =
        a.Values
        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey (k2, k3))
        |> Map.map (fun (k1, k2, k3) v -> v * b.[k2, k3])
        |> SMap3

    static member inline (.*) (b:SMap2<_,_,_>, a:SMap3<_,_,_,_>) =
        a.Values
        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey (k1, k2))
        |> Map.map (fun (k1, k2, k3) v -> v * b.[k1, k2])
        |> SMap3

    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap<_,_>) =
        a.Values
        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey k3)
        |> Map.map (fun (k1, k2, k3) v -> v * b.[k3])
        |> SMap3

    static member inline (.*) (b:SMap<_,_>, a:SMap3<_,_,_,_>) =
        a.Values
        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey k1)
        |> Map.map (fun (k1, k2, k3) v -> v * b.[k1])
        |> SMap3

    static member inline (+) (lhs:SMap3<_,_,_,_>, rhs:SMap3<_,_,_,_>) =
        match Map.count lhs.Values > Map.count rhs.Values with
        | true ->  mergeAddition lhs.Values rhs.Values
        | false -> mergeAddition rhs.Values lhs.Values
        |> SMap3

    static member inline Sum (m:SMap3<_,_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap3<_,_,_,Flips.Types.Decision>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

    //static member inline Sum (m:SMap3<_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
    //    m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


    //module SMap3 =

    //    let ofMap m =
    //        m |> SMap3

    //    let toMap (m:SMap3<_,_,_,_>) =
    //        m.Values

    //    let ofList m =
    //        m |> Map.ofList |> SMap3

    //    let toList (m:SMap3<_,_,_,_>) =
    //        m.Values |> Map.toList

    //    let ofSeq m =
    //        m |> Map.ofSeq |> SMap3

    //    let toSeq (m:SMap3<_,_,_,_>) =
    //        m.Values |> Map.toSeq

    //    let ofArray m =
    //        m |> Map.ofArray |> SMap3

    //    let toArray (m:SMap3<_,_,_,_>) =
    //        m.Values |> Map.toArray

    //    let containsKey k (m:SMap3<_,_,_,_>) =
    //        Map.containsKey k m.Values

    //    let inline reKey f m =
    //        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq