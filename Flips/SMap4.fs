namespace Flips.SliceMap

open System.Collections.Generic
open Utilities

//type SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Value : equality> (m:Map<('Key1 * 'Key2 * 'Key3 * 'Key4),'Value>) =

//    member this.TryFind = m

//    override this.ToString() =
//        sprintf "SMap4 %O" this.TryFind

//    override this.Equals(obj) =
//        match obj with
//        | :? SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> as s -> this.TryFind = s.Values
//        | _ -> false

//    override this.GetHashCode () =
//        hash this.TryFind

//    member this.ContainsKey k =
//        Map.containsKey k this.TryFind

//    member this.AsMap =
//        this.TryFind

//    // Filter Values
//    member private this.FilterValues k1f k2f k3f k4f =
//        let k1Filter = SliceFilterBuilder k1f
//        let k2Filter = SliceFilterBuilder k2f
//        let k3Filter = SliceFilterBuilder k3f
//        let k4Filter = SliceFilterBuilder k4f
        
//        this.TryFind
//        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Filter k1 && k2Filter k2 && k3Filter k3 && k4Filter k4)
//        |> Map.toSeq

//    // Slices
//    // 4D
//    member this.Item
//        with get (k1f, k2f, k3f, k4f) =
//            this.FilterValues k1f k2f k3f k4f |> Map.ofSeq |> SMap4

//    // 3D
//    member this.Item
//        with get (k1, k2f, k3f, k4f) =
//            this.FilterValues (Equals k1) k2f k3f k4f
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k2, k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2, k3f, k4f) =
//            this.FilterValues k1f (Equals k2) k3f k4f
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2f, k3, k4f) =
//            this.FilterValues k1f k2f (Equals k3) k4f
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k2, k4), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2f, k3f, k4) =
//            this.FilterValues k1f k2f k3f (Equals k4) 
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k2, k3), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    // 2D
//    member this.Item
//        with get (k1, k2, k3f, k4f) =
//            this.FilterValues (Equals k1) (Equals k2) k3f k4f
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1, k2f, k3, k4f) =
//            this.FilterValues (Equals k1) k2f (Equals k3) k4f
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k2, k4), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1, k2f, k3f, k4) =
//            this.FilterValues (Equals k1) k2f k3f (Equals k4) 
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2, k3f, k4) =
//            this.FilterValues k1f (Equals k2) k3f (Equals k4) 
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k3), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2f, k3, k4) =
//            this.FilterValues k1f k2f (Equals k3) (Equals k4) 
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k2), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    // 1D
//    member this.Item
//        with get (k1, k2, k3, k4f) =
//            this.FilterValues (Equals k1) (Equals k2) (Equals k3) k4f
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> k4, v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1, k2, k3f, k4) =
//            this.FilterValues (Equals k1) (Equals k2) k3f (Equals k4) 
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> k3, v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1, k2f, k3, k4) =
//            this.FilterValues (Equals k1) k2f (Equals k3) (Equals k4) 
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> k2, v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1f, k2, k3, k4) =
//            this.FilterValues k1f (Equals k2) (Equals k3) (Equals k4) 
//            |> Seq.map (fun ((k1, k2, k3, k4), v) -> k1, v) 
//            |> Map.ofSeq 
//            |> SMap

//    // 0D (aka GetItem)
//    member this.Item
//        with get(k1, k2, k3, k4) =
//            this.TryFind.[k1, k2, k3, k4] 

//    // Operators
//    static member inline (*) (lhs, rhs:SMap4<_,_,_,_,_>) =
//        rhs.Values
//        |> Map.map (fun k v -> lhs * v)
//        |> SMap4

//    static member inline (*) (lhs:SMap4<_,_,_,_,_>, rhs) =
//        lhs.Values
//        |> Map.map (fun k v -> rhs * v)
//        |> SMap4

//    static member inline (.*) (lhs:SMap4<_,_,_,_,_>, rhs:SMap4<_,_,_,_,_>) =
//        lhs.Values
//        |> Map.filter (fun k _ -> rhs.ContainsKey k)
//        |> Map.map (fun (k1, k2, k3, k4) v -> v * rhs.[k1, k2, k3, k4])
//        |> SMap4

//    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap3<_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey (k2, k3, k4))
//        |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k2, k3, k4])
//        |> SMap4

//    static member inline (.*) (b:SMap3<_,_,_,_>, a:SMap4<_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey (k1, k2, k3))
//        |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k1, k2, k3])
//        |> SMap4

//    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap2<_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey (k3, k4))
//        |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k3, k4])
//        |> SMap4

//    static member inline (.*) (b:SMap2<_,_,_>, a:SMap4<_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey (k1, k2))
//        |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k1, k2])
//        |> SMap4

//    static member inline (.*) (a:SMap4<_,_,_,_,_>, b:SMap<_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey k4)
//        |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k4])
//        |> SMap4

//    static member inline (.*) (b:SMap<_,_>, a:SMap4<_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4) _ -> b.ContainsKey k1)
//        |> Map.map (fun (k1, k2, k3, k4) v -> v * b.[k1])
//        |> SMap4

//    static member inline (+) (lhs:SMap4<_,_,_,_,_>, rhs:SMap4<_,_,_,_,_>) =
//        match Map.count lhs.Values > Map.count rhs.Values with
//        | true ->  mergeAddition lhs.Values rhs.Values
//        | false -> mergeAddition rhs.Values lhs.Values
//        |> SMap4

//    static member inline Sum (m:SMap4<_,_,_,_,_>) =
//        m.Values |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap4<_,_,_,_,Flips.Types.Decision>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap4<_,_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


//module SMap4 =

//    let ofMap m =
//        m |> SMap4

//    let toMap (m:SMap4<_,_,_,_,_>) =
//        m.Values

//    let ofList m =
//        m |> Map.ofList |> SMap4

//    let toList (m:SMap4<_,_,_,_,_>) =
//        m.Values |> Map.toList

//    let ofSeq m =
//        m |> Map.ofSeq |> SMap4

//    let toSeq (m:SMap4<_,_,_,_,_>) =
//        m.Values |> Map.toSeq

//    let ofArray m =
//        m |> Map.ofArray |> SMap4

//    let toArray (m:SMap4<_,_,_,_,_>) =
//        m.Values |> Map.toArray

//    let containsKey k (m:SMap4<_,_,_,_,_>) =
//        Map.containsKey k m.Values

//    let reKey f m =
//        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq
