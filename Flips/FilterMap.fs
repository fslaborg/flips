module Flips.SliceMap

// Declared here so it can be used by any of the MapXD types
let inline private getKeyCheck lb ub =
    match lb, ub with
    | Some lb, Some ub -> fun k1 -> k1 >= lb && k1 <= ub
    | Some lb, None -> fun k1 -> k1 >= lb
    | None, Some ub -> fun k1 -> k1 <= ub
    | None, None -> fun _ -> true


let inline sum< ^a, ^b when ^a: (static member Sum: ^a -> ^b)> (k1: ^a) = 
    ((^a) : (static member Sum: ^a -> ^b) k1)

type SliceType<'a when 'a : comparison> =
    | All
    | Equals of 'a
    | GreaterThan of 'a
    | GreaterOrEqual of 'a
    | LessThan of 'a
    | LessOrEqual of 'a
    | In of Set<'a>
    | Between of 'a * 'a
    | Where of ('a -> bool)


let SliceFilterBuilder<'a when 'a : comparison> (f:SliceType<'a>) =
    match f with
    | All -> fun _ -> true
    | Equals k1 -> fun k2 -> k2 = k1
    | GreaterThan k1 -> fun k2 -> k2 > k1
    | GreaterOrEqual k1 -> fun k2 -> k2 >= k1
    | LessThan k1 -> fun k2 -> k2 < k1
    | LessOrEqual k1 -> fun k2 -> k2 <= k1
    | In k1 -> fun k2 -> Set.contains k2 k1
    | Between (k1, k2) -> fun k3 -> k3 >= k1 && k3 <= k2
    | Where k1 -> k1


type SMap<'Key, 'Value when 'Key : comparison> (m:Map<'Key,'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map1D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    // Filter Values
    member private this.FilterValues k1f =
        let k1Filter = SliceFilterBuilder k1f
            
        this.Values
        |> Map.filter (fun k1 _ -> k1Filter k1 )
        |> Map.toSeq

    // Slices
    // 1D
    member this.Item
        with get (k1f) =
            this.FilterValues k1f |> Map.ofSeq |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k) =
            this.Values.[k] 

    // Operators
    static member inline (*) (lhs, rhs:SMap<_,_>) =
        rhs.Values
        |> Map.map (fun _ v -> lhs * v)
        |> SMap

    static member inline (*) (lhs:SMap<_,_>, rhs) =
        rhs * lhs

    static member inline (.*) (lhs:SMap<_,_>, rhs:SMap<_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun k v -> v * rhs.[k])
        |> SMap

    static member inline Sum (m:SMap<_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd


module SMap =

    let ofList m =
        m |> Map.ofList |> SMap

    let toList (m:SMap<_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> SMap

    let toSeq (m:SMap<_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> SMap

    let toArray (m:SMap<_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:SMap<_,_>) =
        Map.containsKey k m.Values


type SMap2<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison> (m:Map<('Key1 * 'Key2),'Value>) =

    member this.Values = m

    override this.ToString () = 
        sprintf "Map2D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    // Filter Values
    member private this.FilterValues k1f k2f =
        let k1Filter = SliceFilterBuilder k1f
        let k2Filter = SliceFilterBuilder k2f
        
        this.Values
        |> Map.filter (fun (k1, k2) _ -> k1Filter k1 && k2Filter k2)
        |> Map.toSeq

    // Slices
    // 2D
    member this.Item
        with get (k1f, k2f) =
            this.FilterValues k1f k2f |> Map.ofSeq |> SMap2

    // 1D
    member this.Item
        with get (k1, k2f) =
            this.FilterValues (Equals k1) k2f 
            |> Seq.map (fun ((_, k2), v) -> k2, v) 
            |> Map.ofSeq 
            |> SMap

    member this.Item
        with get (k1f, k2) =
            this.FilterValues k1f (Equals k2)
            |> Seq.map (fun ((k1, _), v) -> k1, v)
            |> Map.ofSeq
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k) =
            this.Values.[k] 

    // Operators
    static member inline (*) (lhs, rhs:SMap2<_,_,_>) =
        rhs.Values
        |> Map.map (fun _ v -> v * lhs)
        |> SMap

    static member inline (*) (lhs:SMap2<_,_,_>, rhs) =
        rhs * lhs

    static member inline (.*) (lhs:SMap2<_,_,_>, rhs:SMap2<_,_,_>) =
        let rhs = rhs.Values
        lhs.Values
        |> Map.filter (fun (k1, k2) _ -> rhs.ContainsKey (k1, k2))
        |> Map.map (fun (k1, k2) v -> v * rhs.[(k1, k2)])
        |> SMap2

    static member inline Sum (m:SMap2<_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd


module SMap2 =

    let ofList m =
        m |> Map.ofList |> SMap2

    let toList (m:SMap2<_,_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> SMap2

    let toSeq (m:SMap2<_,_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> SMap2

    let toArray (m:SMap2<_,_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:SMap2<_,_,_>) =
        Map.containsKey k m.Values


type SMap3<'Key1, 'Key2, 'Key3, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison> (m:Map<('Key1 * 'Key2 * 'Key3),'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map3D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    // Filter Values
    member private this.FilterValues k1f k2f k3f =
        let k1Filter = SliceFilterBuilder k1f
        let k2Filter = SliceFilterBuilder k2f
        let k3Filter = SliceFilterBuilder k3f
        
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1Filter k1 && k2Filter k2 && k3Filter k3)
        |> Map.toSeq

    // Slices
    // 3D
    member this.Item
        with get (k1f, k2f, k3f) =
            this.FilterValues k1f k2f k3f |> Map.ofSeq |> SMap3

    // 2D
    member this.Item
        with get (k1, k2f, k3f) =
            this.FilterValues (Equals k1) k2f k3f
            |> Seq.map (fun ((k1, k2, k3), v) -> (k2, k3), v) 
            |> Map.ofSeq 
            |> SMap2

    member this.Item
        with get (k1f, k2, k3f) =
            this.FilterValues k1f (Equals k2) k3f
            |> Seq.map (fun ((k1, k2, k3), v) -> (k1, k3), v) 
            |> Map.ofSeq 
            |> SMap2

    member this.Item
        with get (k1f, k2f, k3) =
            this.FilterValues k1f k2f (Equals k3)
            |> Seq.map (fun ((k1, k2, k3), v) -> (k1, k2), v) 
            |> Map.ofSeq 
            |> SMap2

    // 1D
    member this.Item
        with get (k1, k2, k3f) =
            this.FilterValues (Equals k1) (Equals k2) k3f
            |> Seq.map (fun ((k1, k2, k3), v) -> k3, v) 
            |> Map.ofSeq 
            |> SMap

    member this.Item
        with get (k1, k2f, k3) =
            this.FilterValues (Equals k1) k2f (Equals k3)
            |> Seq.map (fun ((k1, k2, k3), v) -> k2, v) 
            |> Map.ofSeq 
            |> SMap

    member this.Item
        with get (k1f, k2, k3) =
            this.FilterValues k1f (Equals k2) (Equals k3)
            |> Seq.map (fun ((k1, k2, k3), v) -> k1, v) 
            |> Map.ofSeq 
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k) =
            this.Values.[k] 

    // Operators
    static member inline (*) (lhs, rhs:SMap3<_,_,_,_>) =
        rhs.Values
        |> Map.map (fun k v -> lhs * v)
        |> SMap3

    static member inline (*) (lhs:SMap3<_,_,_,_>, rhs) =
        rhs * lhs

    static member inline (.*) (lhs:SMap3<_,_,_,_>, rhs:SMap3<_,_,_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun k v -> v * rhs.[k])
        |> SMap3

    static member inline Sum (m:SMap3<_,_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd


module SMap3 =

    let ofList m =
        m |> Map.ofList |> SMap3

    let toList (m:SMap3<_,_,_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> SMap3

    let toSeq (m:SMap3<_,_,_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> SMap3

    let toArray (m:SMap3<_,_,_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:SMap3<_,_,_,_>) =
        Map.containsKey k m.Values


type SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison> (m:Map<('Key1 * 'Key2 * 'Key3 * 'Key4),'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map4D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    // Filter Values
    member private this.FilterValues k1f k2f k3f k4f =
        let k1Filter = SliceFilterBuilder k1f
        let k2Filter = SliceFilterBuilder k2f
        let k3Filter = SliceFilterBuilder k3f
        let k4Filter = SliceFilterBuilder k4f
        
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Filter k1 && k2Filter k2 && k3Filter k3 && k4Filter k4)
        |> Map.toSeq

    // Slices
    // 4D
    member this.Item
        with get (k1f, k2f, k3f, k4f) =
            this.FilterValues k1f k2f k3f k4f |> Map.ofSeq |> SMap4

    // 3D
    member this.Item
        with get (k1, k2f, k3f, k4f) =
            this.FilterValues (Equals k1) k2f k3f k4f
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k2, k3, k4), v) 
            |> Map.ofSeq 
            |> SMap3

    member this.Item
        with get (k1f, k2, k3f, k4f) =
            this.FilterValues k1f (Equals k2) k3f k4f
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k3, k4), v) 
            |> Map.ofSeq 
            |> SMap3

    member this.Item
        with get (k1f, k2f, k3, k4f) =
            this.FilterValues k1f k2f (Equals k3) k4f
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k2, k4), v) 
            |> Map.ofSeq 
            |> SMap3

    member this.Item
        with get (k1f, k2f, k3f, k4) =
            this.FilterValues k1f k2f k3f (Equals k4) 
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k2, k3), v) 
            |> Map.ofSeq 
            |> SMap3

    // 2D
    member this.Item
        with get (k1, k2, k3f, k4f) =
            this.FilterValues (Equals k1) (Equals k2) k3f k4f
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k3, k4), v) 
            |> Map.ofSeq 
            |> SMap2

    member this.Item
        with get (k1, k2f, k3, k4f) =
            this.FilterValues (Equals k1) k2f (Equals k3) k4f
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k2, k4), v) 
            |> Map.ofSeq 
            |> SMap2

    member this.Item
        with get (k1, k2f, k3f, k4) =
            this.FilterValues (Equals k1) k2f k3f (Equals k4) 
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k3, k4), v) 
            |> Map.ofSeq 
            |> SMap2

    member this.Item
        with get (k1f, k2, k3f, k4) =
            this.FilterValues k1f (Equals k2) k3f (Equals k4) 
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k3), v) 
            |> Map.ofSeq 
            |> SMap2

    member this.Item
        with get (k1f, k2f, k3, k4) =
            this.FilterValues k1f k2f (Equals k3) (Equals k4) 
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> (k1, k2), v) 
            |> Map.ofSeq 
            |> SMap2

    // 1D
    member this.Item
        with get (k1, k2, k3, k4f) =
            this.FilterValues (Equals k1) (Equals k2) (Equals k3) k4f
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> k4, v) 
            |> Map.ofSeq 
            |> SMap

    member this.Item
        with get (k1, k2, k3f, k4) =
            this.FilterValues (Equals k1) (Equals k2) k3f (Equals k4) 
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> k3, v) 
            |> Map.ofSeq 
            |> SMap

    member this.Item
        with get (k1, k2f, k3, k4) =
            this.FilterValues (Equals k1) k2f (Equals k3) (Equals k4) 
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> k2, v) 
            |> Map.ofSeq 
            |> SMap

    member this.Item
        with get (k1f, k2, k3, k4) =
            this.FilterValues k1f (Equals k2) (Equals k3) (Equals k4) 
            |> Seq.map (fun ((k1, k2, k3, k4), v) -> k1, v) 
            |> Map.ofSeq 
            |> SMap

    // 0D (aka GetItem)
    member this.Item
        with get(k) =
            this.Values.[k] 

    // Operators
    static member inline (*) (lhs, rhs:SMap4<_,_,_,_,_>) =
        rhs.Values
        |> Map.map (fun k v -> lhs * v)
        |> SMap4

    static member inline (*) (lhs:SMap4<_,_,_,_,_>, rhs) =
        rhs * lhs

    static member inline (.*) (lhs:SMap4<_,_,_,_,_>, rhs:SMap4<_,_,_,_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun k v -> v * rhs.[k])
        |> SMap4

    static member inline Sum (m:SMap4<_,_,_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd


module SMap4 =

    let ofList m =
        m |> Map.ofList |> SMap4

    let toList (m:SMap4<_,_,_,_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> SMap4

    let toSeq (m:SMap4<_,_,_,_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> SMap3

    let toArray (m:SMap4<_,_,_,_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:SMap4<_,_,_,_,_>) =
        Map.containsKey k m.Values
