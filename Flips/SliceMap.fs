module Flips.SliceMap

// Declared here so it can be used by any of the MapXD types
let inline private getKeyCheck lb ub =
    match lb, ub with
    | Some lb, Some ub -> fun x -> x >= lb && x <= ub
    | Some lb, None -> fun x -> x >= lb
    | None, Some ub -> fun x -> x <= ub
    | None, None -> fun _ -> true


let inline sum< ^a, ^b when ^a: (static member sum: ^a -> ^b)> (x: ^a) = 
    ((^a) : (static member sum: ^a -> ^b) x)


type Map1D<'Key, 'Value when 'Key : comparison> (m:Map<'Key,'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map1D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.Item
        with get(k) =
            this.Values.[k] 

    member this.GetSlice (lb, ub) = 
        let check = getKeyCheck lb ub
        this.Values
        |> Map.filter (fun k _ -> check k)
        |> Map1D

    static member inline (*) (lhs:Map1D<_,_>, rhs:Map1D<_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun k v -> v * rhs.[k])
        |> Map1D

    static member inline sum (m:Map1D<_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd


module Map1D =

    let inline ofList x =
        x |> Map.ofList |> Map1D

    let inline ofSeq x =
        x |> Map.ofSeq |> Map1D

    let inline ofArray x =
        x |> Map.ofArray |> Map1D

    let containsKey k (m:Map1D<_,_>) =
        Map.containsKey k m.Values


type Map2D<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison> (m:Map<('Key1 * 'Key2),'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map2D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.Item
        with get(k) =
            this.Values.[k] 

    member this.GetSlice (sk1, lb2, ub2) = 
        let k2Check = getKeyCheck lb2 ub2
        this.Values
        |> Map.filter (fun (k1, k2) _ -> k1 = sk1 && k2Check k2)
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> snd k, v)
        |> Map1D.ofSeq

    member this.GetSlice (lb1, ub1, sk2) =
        let k1Check = getKeyCheck lb1 ub1
        this.Values
        |> Map.filter (fun (k1, k2) _ -> k1Check k1 && k2 = sk2)
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> fst k, v)
        |> Map1D.ofSeq

    member this.GetSlice (lb1, ub1, lb2, ub2) =
        let key1Check = getKeyCheck lb1 ub1
        let key2Check = getKeyCheck lb2 ub2
        this.Values
        |> Map.filter (fun (k1, k2) _ -> (key1Check k1) && (key2Check k2)) 
        |> Map2D

    static member inline (*) (lhs, rhs:Map1D<_,_>) =
        rhs.Values
        |> Map.map (fun _ v -> v * lhs)
        |> Map1D

    static member inline (*) (lhs:Map1D<_,_>, rhs) =
        rhs * lhs

    static member inline (.*) (lhs:Map2D<_,_,_>, rhs:Map1D<_,_>) =
        lhs.Values
        |> Map.filter (fun (_, k2) _ -> Map1D.containsKey k2 rhs)
        |> Map.map (fun (_, k2) v -> v * rhs.[k2])
        |> Map2D

    static member inline (.*) (lhs:Map1D<_,_>, rhs:Map2D<_,_,_>) =
        rhs.Values
        |> Map.filter (fun (k1, _) _ -> Map1D.containsKey k1 lhs)
        |> Map.map (fun (k1, _) v -> v * lhs.[k1])
        |> Map2D

    static member inline (.*) (lhs:Map2D<_,_,_>, rhs:Map2D<_,_,_>) =
        lhs.Values
        |> Map.filter (fun (k1, k2) _ -> rhs.ContainsKey (k1, k2))
        |> Map.map (fun (k1, k2) v -> v * rhs.[(k1, k2)])
        |> Map2D

    static member inline sum (m:Map2D<_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd

    static member inline toSeq (m:Map2D<_,_,_>) =
        m.Values
        |> Map.toSeq


module Map2D =

    let ofList m =
        m |> Map.ofList |> Map2D

    let ofSeq m =
        m |> Map.ofSeq |> Map2D

    let ofArray m =
        m |> Map.ofArray |> Map2D

    let containsKey k (m:Map2D<_,_,_>) =
        Map.containsKey k m.Values


type Map3D<'Key1, 'Key2, 'Key3, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison> (m:Map<('Key1 * 'Key2 * 'Key3),'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map3D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.Item
        with get(k) =
            this.Values.[k] 

    member this.GetSlice (lb1, ub1, sk2, sk3) = 
        let k1Check = getKeyCheck lb1 ub1
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1Check k1 && k2 = sk2 && k3 = sk3)
        |> Map.toSeq
        |> Seq.map (fun ((k1, _, _), v) -> k1, v )
        |> Map.ofSeq
        |> Map1D

    member this.GetSlice (sk1, lb2, ub2, sk3) = 
        let k2Check = getKeyCheck lb2 ub2
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1 = sk1 && k2Check k2 && k3 = sk3)
        |> Map.toSeq
        |> Seq.map (fun ((_, k2, _), v) -> k2, v )
        |> Map.ofSeq
        |> Map1D

    member this.GetSlice (sk1, sk2, lb3, ub3) = 
        let k3Check = getKeyCheck lb3 ub3
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1 = sk1 && k2 = sk2 && k3Check k3)
        |> Map.toSeq
        |> Seq.map (fun ((_, _, k3), v) -> k3, v )
        |> Map.ofSeq
        |> Map1D

    member this.GetSlice (sk1, lb2, ub2, lb3, ub3) = 
        let k2Check = getKeyCheck lb2 ub2
        let k3Check = getKeyCheck lb3 ub3
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1 = sk1 && k2Check k2 && k3Check k3)
        |> Map.toSeq
        |> Seq.map (fun ((_, k2, k3), v) -> (k2, k3), v )
        |> Map.ofSeq
        |> Map2D

    member this.GetSlice (lb1, ub1, sk2, lb3, ub3) =
        let k1Check = getKeyCheck lb1 ub1
        let k3Check = getKeyCheck lb3 ub3
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1Check k1 && k2 = sk2 && k3Check k3)
        |> Map.toSeq
        |> Seq.map (fun ((k1, _, k3), v) -> (k1, k3), v)
        |> Map.ofSeq
        |> Map2D

    member this.GetSlice (lb1, ub1, lb2, ub2, sk3) =
        let k1Check = getKeyCheck lb1 ub1
        let k2Check = getKeyCheck lb2 ub2
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1Check k1 && k2Check k2 && k3 = sk3)
        |> Map.toSeq
        |> Seq.map (fun ((k1, k2, _), v) -> (k1, k2), v)
        |> Map.ofSeq
        |> Map2D

    member this.GetSlice (lb1, ub1, lb2, ub2, lb3, ub3) =
        let k1Check = getKeyCheck lb1 ub1
        let k2Check = getKeyCheck lb2 ub2
        let k3Check = getKeyCheck lb3 ub3
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1Check k1 && k2Check k2 && k3Check k3)
        |> Map3D

    static member inline (*) (lhs, rhs:Map2D<_,_,_>) =
        rhs.Values
        |> Map.map (fun k v -> lhs * v)
        |> Map2D

    static member inline (*) (lhs:Map2D<_,_,_>, rhs) =
        lhs.Values
        |> Map.map (fun _ v -> v * rhs)
        |> Map2D

    static member inline (.*) (lhs:Map3D<_,_,_,_>, rhs) =
        lhs.Values
        |> Map.map (fun _ v -> v * rhs)
        |> Map3D

    static member inline (.*) (lhs, rhs:Map3D<_,_,_,_>) =
        rhs.Values
        |> Map.map (fun _ v -> v * lhs)
        |> Map3D

    static member inline (.*) (lhs:Map3D<_,_,_,_>, rhs:Map3D<_,_,_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun k v -> v * rhs.[k])
        |> Map3D

    static member inline sum (m:Map3D<_,_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd

    static member inline toSeq (m:Map2D<_,_,_>) =
        m.Values
        |> Map.toSeq


module Map3D =

    let ofList m =
        m |> Map.ofList |> Map3D

    let ofSeq m =
        m |> Map.ofSeq |> Map3D

    let ofArray m =
        m |> Map.ofArray |> Map3D

    let containsKey k (m:Map3D<_,_,_,_>) =
        Map.containsKey k m.Values
