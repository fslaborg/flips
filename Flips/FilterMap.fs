module Flips.FilterMap

// Declared here so it can be used by any of the MapXD types
let inline private getKeyCheck lb ub =
    match lb, ub with
    | Some lb, Some ub -> fun x -> x >= lb && x <= ub
    | Some lb, None -> fun x -> x >= lb
    | None, Some ub -> fun x -> x <= ub
    | None, None -> fun _ -> true


let inline sum< ^a, ^b when ^a: (static member Sum: ^a -> ^b)> (x: ^a) = 
    ((^a) : (static member Sum: ^a -> ^b) x)

type FilterType<'a when 'a : comparison> =
    | Equals of 'a
    | GreaterThan of 'a
    | GreaterOrEqual of 'a
    | LessThan of 'a
    | LessOrEqual of 'a
    | In of Set<'a>
    | Between of 'a * 'a
    | Where of ('a -> bool)

let (.=) x = Equals x
let (.>) x = GreaterThan x
let (.>=) x = GreaterOrEqual x
let (.<) x = LessThan x
let (.<=) x = LessOrEqual x
let In x = In x
let Btwn x y = Between (x, y)
let Where f = Where f

let FilterBuilder<'a when 'a : comparison> (f:FilterType<'a>) =
    match f with
    | Equals x -> (=) x
    | GreaterThan x -> (>) x
    | GreaterOrEqual x -> (>=) x
    | LessThan x -> (<) x
    | LessOrEqual x -> (<=) x
    | In x -> fun y -> Set.contains y x
    | Between (x, y) -> fun z -> z >= x && z <= y
    | Where x -> x


type FMap<'Key, 'Value when 'Key : comparison> (m:Map<'Key,'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map1D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.Item
        with get(k) =
            this.Values.[k] 

    // 1D Slice
    member this.GetSlice (lb, ub) = 
        let check = getKeyCheck lb ub
        this.Values
        |> Map.filter (fun k _ -> check k)
        |> FMap

    // Operators
    static member inline (*) (lhs, rhs:FMap<_,_>) =
        rhs.Values
        |> Map.map (fun _ v -> lhs * v)
        |> FMap

    static member inline (*) (lhs:FMap<_,_>, rhs) =
        lhs.Values
        |> Map.map (fun _ v -> rhs * v)
        |> FMap

    static member inline (.*) (lhs:FMap<_,_>, rhs:FMap<_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun k v -> v * rhs.[k])
        |> FMap

    static member inline Sum (m:FMap<_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd


module FMap =

    let ofList m =
        m |> Map.ofList |> FMap

    let toList (m:FMap<_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> FMap

    let toSeq (m:FMap<_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> FMap

    let toArray (m:FMap<_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:FMap<_,_>) =
        Map.containsKey k m.Values


type FMap2<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison> (m:Map<('Key1 * 'Key2),'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map2D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.Item
        with get(k) =
            this.Values.[k] 

    // Filter Values
    member private this.FilterValues f1 f2 =
        let k1Filter = FilterBuilder f1
        let k2Filter = FilterBuilder f2
        
        this.Values
        |> Map.filter (fun (k1, k2) _ -> k1Filter k1 && k2Filter k2)
        |> Map.toSeq

    // Slices
    member this.GetSlice (f1, f2) =
        this.FilterValues f1 f2 |> Map.ofSeq |> FMap2

    member this.GetSlice (x, f2) =
        this.FilterValues (Equals x) f2 
        |> Seq.map (fun ((_, k2), v) -> k2, v) 
        |> Map.ofSeq 
        |> FMap

    member this.GetSlice (f1, x) =
        this.FilterValues f1 (Equals x)
        |> Seq.map (fun ((k1, _), v) -> k1, v)
        |> Map.ofSeq
        |> FMap


    // Operators
    static member inline (*) (lhs, rhs:FMap<_,_>) =
        rhs.Values
        |> Map.map (fun _ v -> v * lhs)
        |> FMap

    static member inline (*) (lhs:FMap<_,_>, rhs) =
        rhs * lhs

    static member inline (.*) (lhs:FMap2<_,_,_>, rhs:FMap<_,_>) =
        lhs.Values
        |> Map.filter (fun (_, k2) _ -> FMap.containsKey k2 rhs)
        |> Map.map (fun (_, k2) v -> v * rhs.[k2])
        |> FMap2

    static member inline (.*) (lhs:FMap<_,_>, rhs:FMap2<_,_,_>) =
        rhs.Values
        |> Map.filter (fun (k1, _) _ -> FMap.containsKey k1 lhs)
        |> Map.map (fun (k1, _) v -> v * lhs.[k1])
        |> FMap2

    static member inline (.*) (lhs:FMap2<_,_,_>, rhs:FMap2<_,_,_>) =
        lhs.Values
        |> Map.filter (fun (k1, k2) _ -> rhs.ContainsKey (k1, k2))
        |> Map.map (fun (k1, k2) v -> v * rhs.[(k1, k2)])
        |> FMap2

    static member inline Sum (m:FMap2<_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd


module FMap2 =

    let ofList m =
        m |> Map.ofList |> FMap2

    let toList (m:FMap2<_,_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> FMap2

    let toSeq (m:FMap2<_,_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> FMap2

    let toArray (m:FMap2<_,_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:FMap2<_,_,_>) =
        Map.containsKey k m.Values


type FMap3<'Key1, 'Key2, 'Key3, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison> (m:Map<('Key1 * 'Key2 * 'Key3),'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map3D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.Item
        with get(k) =
            this.Values.[k] 

    // Filter Values
    member private this.FilterValues f1 f2 f3 =
        let k1Filter = FilterBuilder f1
        let k2Filter = FilterBuilder f2
        let k3Filter = FilterBuilder f3
        
        this.Values
        |> Map.filter (fun (k1, k2, k3) _ -> k1Filter k1 && k2Filter k2 && k3Filter k3)
        |> Map.toSeq

    // Slices
    member this.GetSlice (f1, f2, f3) =
        this.FilterValues f1 f2 f3 |> Map.ofSeq |> FMap3

    //// 1D Slices
    //member this.GetSlice (lb1, ub1, sk2, sk3) = 
    //    let k1Check = getKeyCheck lb1 ub1
    //    this.Values
    //    |> Map.filter (fun (k1, k2, k3) _ -> k1Check k1 && k2 = sk2 && k3 = sk3)
    //    |> Map.toSeq
    //    |> Seq.map (fun ((k1, _, _), v) -> k1, v )
    //    |> Map.ofSeq
    //    |> FMap

    //member this.GetSlice (sk1, lb2, ub2, sk3) = 
    //    let k2Check = getKeyCheck lb2 ub2
    //    this.Values
    //    |> Map.filter (fun (k1, k2, k3) _ -> k1 = sk1 && k2Check k2 && k3 = sk3)
    //    |> Map.toSeq
    //    |> Seq.map (fun ((_, k2, _), v) -> k2, v )
    //    |> Map.ofSeq
    //    |> FMap

    //member this.GetSlice (sk1, sk2, lb3, ub3) = 
    //    let k3Check = getKeyCheck lb3 ub3
    //    this.Values
    //    |> Map.filter (fun (k1, k2, k3) _ -> k1 = sk1 && k2 = sk2 && k3Check k3)
    //    |> Map.toSeq
    //    |> Seq.map (fun ((_, _, k3), v) -> k3, v )
    //    |> Map.ofSeq
    //    |> FMap

    //// 2D Slices
    //member this.GetSlice (sk1, lb2, ub2, lb3, ub3) = 
    //    let k2Check = getKeyCheck lb2 ub2
    //    let k3Check = getKeyCheck lb3 ub3
    //    this.Values
    //    |> Map.filter (fun (k1, k2, k3) _ -> k1 = sk1 && k2Check k2 && k3Check k3)
    //    |> Map.toSeq
    //    |> Seq.map (fun ((_, k2, k3), v) -> (k2, k3), v )
    //    |> Map.ofSeq
    //    |> FMap2

    //member this.GetSlice (lb1, ub1, sk2, lb3, ub3) =
    //    let k1Check = getKeyCheck lb1 ub1
    //    let k3Check = getKeyCheck lb3 ub3
    //    this.Values
    //    |> Map.filter (fun (k1, k2, k3) _ -> k1Check k1 && k2 = sk2 && k3Check k3)
    //    |> Map.toSeq
    //    |> Seq.map (fun ((k1, _, k3), v) -> (k1, k3), v)
    //    |> Map.ofSeq
    //    |> FMap2

    //member this.GetSlice (lb1, ub1, lb2, ub2, sk3) =
    //    let k1Check = getKeyCheck lb1 ub1
    //    let k2Check = getKeyCheck lb2 ub2
    //    this.Values
    //    |> Map.filter (fun (k1, k2, k3) _ -> k1Check k1 && k2Check k2 && k3 = sk3)
    //    |> Map.toSeq
    //    |> Seq.map (fun ((k1, k2, _), v) -> (k1, k2), v)
    //    |> Map.ofSeq
    //    |> FMap2

    //// 3D Slice
    //member this.GetSlice (lb1, ub1, lb2, ub2, lb3, ub3) =
    //    let k1Check = getKeyCheck lb1 ub1
    //    let k2Check = getKeyCheck lb2 ub2
    //    let k3Check = getKeyCheck lb3 ub3
    //    this.Values
    //    |> Map.filter (fun (k1, k2, k3) _ -> k1Check k1 && k2Check k2 && k3Check k3)
    //    |> FMap3

    // Operators
    static member inline (*) (lhs, rhs:FMap3<_,_,_,_>) =
        rhs.Values
        |> Map.map (fun k v -> lhs * v)
        |> FMap3

    static member inline (*) (lhs:FMap3<_,_,_,_>, rhs) =
        lhs.Values
        |> Map.map (fun _ v -> v * rhs)
        |> FMap3

    static member inline (.*) (lhs:FMap3<_,_,_,_>, rhs:FMap3<_,_,_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun k v -> v * rhs.[k])
        |> FMap3

    static member inline Sum (m:FMap3<_,_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd


module FMap3 =

    let ofList m =
        m |> Map.ofList |> FMap3

    let toList (m:FMap3<_,_,_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> FMap3

    let toSeq (m:FMap3<_,_,_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> FMap3

    let toArray (m:FMap3<_,_,_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:FMap3<_,_,_,_>) =
        Map.containsKey k m.Values


type FMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison> (m:Map<('Key1 * 'Key2 * 'Key3 * 'Key4),'Value>) =

    member this.Values = m

    override this.ToString() =
        sprintf "Map4D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.Item
        with get(k) =
            this.Values.[k] 

    // 1D Slices
    member this.GetSlice (lb1, ub1, sk2, sk3, sk4) = 
        let k1Check = getKeyCheck lb1 ub1
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Check k1 && k2 = sk2 && k3 = sk3 && k4 = sk4)
        |> Map.toSeq
        |> Seq.map (fun ((k1, _, _, _), v) -> k1, v )
        |> Map.ofSeq
        |> FMap

    member this.GetSlice (sk1, lb2, ub2, sk3, sk4) = 
        let k2Check = getKeyCheck lb2 ub2
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1 = sk1 && k2Check k2 && k3 = sk3 && k4 = sk4)
        |> Map.toSeq
        |> Seq.map (fun ((_, k2, _, _), v) -> k2, v )
        |> Map.ofSeq
        |> FMap

    member this.GetSlice (sk1, sk2, lb3, ub3, sk4) = 
        let k3Check = getKeyCheck lb3 ub3
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1 = sk1 && k2 = sk2 && k3Check k3 && k4 = sk4)
        |> Map.toSeq
        |> Seq.map (fun ((_, _, k3, _), v) -> k3, v )
        |> Map.ofSeq
        |> FMap

    member this.GetSlice (sk1, sk2, sk3, lb4, ub4) = 
        let k4Check = getKeyCheck lb4 ub4
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1 = sk1 && k2 = sk2 && k3 = sk3 && k4Check k4)
        |> Map.toSeq
        |> Seq.map (fun ((_, _, _, k4), v) -> k4, v )
        |> Map.ofSeq
        |> FMap

    // 2D Slices
    member this.GetSlice (sk1, sk2, lb3, ub3, lb4, ub4) = 
        let k3Check = getKeyCheck lb3 ub3
        let k4Check = getKeyCheck lb4 ub4
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1 = sk1 && k2 = sk2 && k3Check k3 && k4Check k4)
        |> Map.toSeq
        |> Seq.map (fun ((_, _, k3, k4), v) -> (k3, k4), v )
        |> Map.ofSeq
        |> FMap2

    member this.GetSlice (sk1, lb2, ub2, sk3, lb4, ub4) = 
        let k2Check = getKeyCheck lb2 ub2
        let k4Check = getKeyCheck lb4 ub4
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1 = sk1 && k2Check k2 && k3 = sk3 && k4Check k4)
        |> Map.toSeq
        |> Seq.map (fun ((_, k2, _, k4), v) -> (k2, k4), v )
        |> Map.ofSeq
        |> FMap2

    member this.GetSlice (lb1, ub1, sk2, sk3, lb4, ub4) =
        let k1Check = getKeyCheck lb1 ub1
        let k4Check = getKeyCheck lb4 ub4
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Check k1 && k2 = sk2 && k3 = sk3 && k4Check k4)
        |> Map.toSeq
        |> Seq.map (fun ((k1, _, _, k4), v) -> (k1, k4), v)
        |> Map.ofSeq
        |> FMap2

    member this.GetSlice (lb1, ub1, sk2, lb3, ub3, sk4) =
        let k1Check = getKeyCheck lb1 ub1
        let k3Check = getKeyCheck lb3 ub3
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Check k1 && k2 = sk2 && k3Check k3 && k4 = sk4)
        |> Map.toSeq
        |> Seq.map (fun ((k1, _, k3, _), v) -> (k1, k3), v)
        |> Map.ofSeq
        |> FMap2

    member this.GetSlice (lb1, ub1, lb2, ub2, sk3, sk4) =
        let k1Check = getKeyCheck lb1 ub1
        let k2Check = getKeyCheck lb2 ub2
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Check k1 && k2Check k2 && k3 = sk3 && k4 = sk4)
        |> Map.toSeq
        |> Seq.map (fun ((k1, k2, _, _), v) -> (k1, k2), v)
        |> Map.ofSeq
        |> FMap2

    // 3D Slices
    member this.GetSlice (lb1, ub1, lb2, ub2, lb3, ub3, sk4) =
        let k1Check = getKeyCheck lb1 ub1
        let k2Check = getKeyCheck lb2 ub2
        let k3Check = getKeyCheck lb3 ub3
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Check k1 && k2Check k2 && k3Check k3 && k4 = sk4)
        |> Map.toSeq
        |> Seq.map (fun ((k1, k2, k3, _), v) -> (k1, k2, k3), v)
        |> Map.ofSeq
        |> FMap3

    member this.GetSlice (lb1, ub1, lb2, ub2, sk3, lb4, ub4) =
        let k1Check = getKeyCheck lb1 ub1
        let k2Check = getKeyCheck lb2 ub2
        let k4Check = getKeyCheck lb4 ub4
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Check k1 && k2Check k2 && k3 = sk3 && k4Check k4)
        |> Map.toSeq
        |> Seq.map (fun ((k1, k2, _, k4), v) -> (k1, k2, k4), v)
        |> Map.ofSeq
        |> FMap3

    member this.GetSlice (lb1, ub1, sk2, lb3, ub3, lb4, ub4) =
        let k1Check = getKeyCheck lb1 ub1
        let k3Check = getKeyCheck lb3 ub3
        let k4Check = getKeyCheck lb4 ub4
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Check k1 && k2 = sk2 && k3Check k3 && k4Check k4)
        |> Map.toSeq
        |> Seq.map (fun ((k1, _, k3, k4), v) -> (k1, k3, k4), v)
        |> Map.ofSeq
        |> FMap3

    member this.GetSlice (sk1, lb2, ub2, lb3, ub3, lb4, ub4) =
        let k2Check = getKeyCheck lb2 ub2
        let k3Check = getKeyCheck lb3 ub3
        let k4Check = getKeyCheck lb4 ub4
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1 = sk1 && k2Check k2 &&  k3Check k3 && k4Check k4)
        |> Map.toSeq
        |> Seq.map (fun ((_, k2, k3, k4), v) -> (k2, k3, k4), v)
        |> Map.ofSeq
        |> FMap3

    // 4D Slice
    member this.GetSlice (lb1, ub1, lb2, ub2, lb3, ub3, lb4, ub4) =
        let k1Check = getKeyCheck lb1 ub1
        let k2Check = getKeyCheck lb2 ub2
        let k3Check = getKeyCheck lb3 ub3
        let k4Check = getKeyCheck lb4 ub4
        this.Values
        |> Map.filter (fun (k1, k2, k3, k4) _ -> k1Check k1 && k2Check k2 && k3Check k3 && k4Check k4)
        |> FMap4

    // Operators
    static member inline (*) (lhs, rhs:FMap4<_,_,_,_,_>) =
        rhs.Values
        |> Map.map (fun k v -> lhs * v)
        |> FMap4

    static member inline (*) (lhs:FMap4<_,_,_,_,_>, rhs) =
        lhs.Values
        |> Map.map (fun _ v -> v * rhs)
        |> FMap4

    static member inline (.*) (lhs:FMap4<_,_,_,_,_>, rhs:FMap4<_,_,_,_,_>) =
        lhs.Values
        |> Map.filter (fun k _ -> rhs.ContainsKey k)
        |> Map.map (fun k v -> v * rhs.[k])
        |> FMap4

    static member inline Sum (m:FMap4<_,_,_,_,_>) =
        m.Values |> Map.toSeq |> Seq.sumBy snd


module FMap4 =

    let ofList m =
        m |> Map.ofList |> FMap4

    let toList (m:FMap4<_,_,_,_,_>) =
        m.Values |> Map.toList

    let ofSeq m =
        m |> Map.ofSeq |> FMap4

    let toSeq (m:FMap4<_,_,_,_,_>) =
        m.Values |> Map.toSeq

    let ofArray m =
        m |> Map.ofArray |> FMap3

    let toArray (m:FMap4<_,_,_,_,_>) =
        m.Values |> Map.toArray

    let containsKey k (m:FMap4<_,_,_,_,_>) =
        Map.containsKey k m.Values
