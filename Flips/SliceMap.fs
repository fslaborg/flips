namespace Flips.SliceMap

open System.Collections.Generic


type IStore<'Key, 'Value> =
  abstract member GetItem : 'Key -> 'Value
  abstract member TryGetItem : 'Key -> 'Value option



type SliceType<'a when 'a : comparison> =
    | All
    | Equals of 'a
    | GreaterThan of 'a
    | GreaterOrEqual of 'a
    | LessThan of 'a
    | LessOrEqual of 'a
    | Between of 'a * 'a
    | In of Set<'a>
    | NotIn of Set<'a>
    | Where of ('a -> bool)

[<AutoOpen>]
module Utilities =

    // Declared here so it can be used by any of the MapXD types
    let inline internal getKeyCheck lb ub =
        match lb, ub with
        | Some lb, Some ub -> fun k1 -> k1 >= lb && k1 <= ub
        | Some lb, None -> fun k1 -> k1 >= lb
        | None, Some ub -> fun k1 -> k1 <= ub
        | None, None -> fun _ -> true


    let inline internal mergeAddition (lhs:Map<_,_>) (rhs:Map<_,_>) =
        /// The assumption is that the LHS Map has more entries than the RHS Map
        let newRhsValues = rhs |> Map.filter (fun k _ -> not (lhs.ContainsKey k)) |> Map.toSeq

        lhs
        |> Map.map (fun k lhsV -> match Map.tryFind k rhs with 
                                  | Some rhsV -> lhsV + rhsV 
                                  | None -> lhsV)
        |> fun newLhs -> Seq.fold (fun m (k, v) -> Map.add k v m) newLhs newRhsValues


    let inline internal filterKeys (f:SliceType<'a>) (keys:Set<'a>) : Set<'a> =
        match f with
        | All -> keys
        | Equals k -> Set.add k Set.empty
        | GreaterThan k -> Set.filter (fun x -> x > k) keys
        | GreaterOrEqual k -> Set.filter (fun x -> x >= k) keys
        | LessThan k -> Set.filter (fun x -> x < k) keys
        | LessOrEqual k -> Set.filter (fun x -> x <= k) keys
        | Between (lowerBound, upperBound) -> Set.filter (fun x -> x >= lowerBound && x <= upperBound) keys
        | In set -> Set.intersect set keys
        | NotIn set -> Set.difference keys set
        | Where f -> Set.filter f keys

    let inline sum< ^a, ^b when ^a: (static member Sum: ^a -> ^b)> (k1: ^a) = 
        ((^a) : (static member Sum: ^a -> ^b) k1)

    let inline sumAll< ^a, ^b when ^a: (static member Sum: ^a -> ^b) 
                              and ^a: (static member (+): ^a * ^a -> ^a)
                              and ^a: (static member Zero: ^a)> (k1: ^a seq) = 
        let r = Seq.sum k1
        ((^a) : (static member Sum: ^a -> ^b) r)


module internal IStore =

    let create getItem tryGetItem =
        { new IStore<'Key, 'Value> with
            member this.GetItem k = getItem k
            member this.TryGetItem k = tryGetItem k
        }

    let ofDictionary (d:Dictionary<'Key,'Value>) =
        { new IStore<'Key, 'Value> with

            member this.GetItem k = d.[k]

            member this.TryGetItem k = 
                match d.TryGetValue(k) with
                | true, value -> Some value
                | false, _ -> None
        }

    let ofSeq (s:seq<'Key * 'Value>) =
        s 
        |> Seq.map (fun (k, v) -> KeyValuePair(k, v)) 
        |> Dictionary
        |> ofDictionary

    let toSeq (keys:seq<_>) (s:IStore<_,_>) =
        let tryGet k =
            s.TryGetItem(k) |> Option.map (fun v -> k, v)
        
        keys
        |> Seq.choose tryGet

    let toMap keys (s:IStore<_,_>) =
        s |> (toSeq keys) |> Map.ofSeq


    let inline scale coef keys (d:IStore<_,_>) =
        let newDict = new Dictionary<_,_>()
      
        for k in keys do
            match d.TryGetItem(k) with
            | Some v ->
                newDict.[k] <- v * coef
            | None -> ()

        ofDictionary newDict


    let inline mergeAdd (keys:seq<_>) (a:IStore<_,_>) (b:IStore<_,_>) =
        let newDict = new Dictionary<_,_>()

        for key in keys do
            match a.TryGetItem(key), b.TryGetItem(key) with
            | Some lValue, Some rValue -> 
                newDict.Add(key, lValue + rValue)
            | Some lValue, None -> 
                newDict.Add(key, lValue)
            | None, Some rValue ->
                newDict.Add(key, rValue)
            | None, None->
                ()

        ofDictionary newDict


    let inline multiply (keys:seq<_>) (a:IStore<_,_>) keyBuilder (b:IStore<_,_>) =
        let newDict = new Dictionary<_,_>()

        for key in keys do
            match a.TryGetItem(key), b.TryGetItem(keyBuilder key) with
            | Some lValue, Some rValue -> 
                newDict.Add(key, lValue * rValue)
            | _, _ ->
                ()

        ofDictionary newDict


    let inline sum keys (d:IStore<_,_>) =
        let mutable acc = LanguagePrimitives.GenericZero

        for k in keys do
            acc <- acc + d.GetItem(k)

        acc


    let sumDecisions keys (d:IStore<_,Flips.Types.Decision>) =
        let mutable acc = LanguagePrimitives.GenericZero
        
        for k in keys do
            acc <- acc + (1.0 * d.GetItem(k))

        acc


    let sumDecisionsWithUnits keys (d:IStore<_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
        let mutable acc = LanguagePrimitives.GenericZero
      
        for k in keys do
            acc <- acc + (1.0 * d.GetItem(k))

        acc



type SMap<'Key, 'Value when 'Key : comparison and 'Value : equality> (keys:Set<'Key>, values:IStore<'Key, 'Value>) =
    member this.Keys = keys
    member this.Values = values

    new (s:seq<'Key * 'Value>) =
        let keys = s |> Seq.map fst |> Set.ofSeq
        let store = IStore.ofSeq s
        SMap (keys, store)

    member this.AsMap =
      this.Values
      |> IStore.toMap this.Keys

    override this.ToString() =
        sprintf "SMap %O" this.AsMap

    override this.Equals(obj) =
        match obj with
        | :? SMap<'Key, 'Value> as s -> 
            this.AsMap = s.AsMap
        | _ -> false

    override this.GetHashCode () =
        hash this.AsMap

    member this.ContainsKey k =
        match this.Values.TryGetItem k with
        | Some _ -> true
        | None -> false

    // Slices
    // 1D
    member this.Item
        with get (k1f) =
            let newKeys = filterKeys k1f this.Keys
            SMap(newKeys, this.Values)

    // 0D (aka GetItem)
    member this.Item
        with get(k) =
            this.Values.GetItem k

    // Operators
    static member inline (*) (coef, smap:SMap<_,_>) =
        let newStore = IStore.scale coef smap.Keys smap.Values
        SMap(smap.Keys, newStore)

    static member inline (*) (smap:SMap<_,_>, coef) =
        let newValues = IStore.scale coef smap.Keys smap.Values
        SMap(smap.Keys, newValues)

    static member inline (.*) (lhs:SMap<_,_>, rhs:SMap<_,_>) =
        let newKeys = Set.intersect lhs.Keys rhs.Keys
        let newValues = IStore.multiply newKeys lhs.Values id rhs.Values
        SMap(newKeys, newValues)

    static member inline (+) (lhs:SMap<_,_>, rhs:SMap<_,_>) =
        let newKeys = lhs.Keys + rhs.Keys
        let newValues = IStore.mergeAdd newKeys lhs.Values rhs.Values
        SMap(newKeys, newValues)

    static member inline Sum (m:SMap<_,_>) =
        IStore.sum m.Keys m.Values

    static member Sum (m:SMap<_,Flips.Types.Decision>) =
        IStore.sumDecisions m.Keys m.Values

    static member Sum (m:SMap<_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
        IStore.sumDecisionsWithUnits m.Keys m.Values


module SMap =

    let ofSeq (m:seq<_>) =
        m |> SMap

    let toSeq (m:SMap<_,_>) =
        IStore.toSeq m.Keys m.Values

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


type SMap2<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Value : equality> (keys1:Set<'Key1>, keys2:Set<'Key2>, values:IStore<('Key1 * 'Key2), 'Value>) =
    member this.Keys1 = keys1
    member this.Keys2 = keys2
    member this.PossibleKeys = seq {for k1 in this.Keys1 do for k2 in this.Keys2 -> (k1, k2)}
    member this.Values = values

    new (s:seq<('Key1 * 'Key2) * 'Value>) =
        let keys1 = s |> Seq.map (fst >> fst) |> Set.ofSeq
        let keys2 = s |> Seq.map (fst >> snd) |> Set.ofSeq
        let store = IStore.ofSeq s
        SMap2 (keys1, keys2, store)

    member this.AsMap =
        this.Values
        |> IStore.toMap this.PossibleKeys

    override this.ToString () = 
        sprintf "SMap2 %O" this.AsMap

    override this.Equals(obj) =
        match obj with
        | :? SMap2<'Key1, 'Key2, 'Value > as s -> 
          this.AsMap = s.AsMap
        | _ -> false

    override this.GetHashCode () =
        hash this.AsMap

    member this.ContainsKey k =
        match this.Values.TryGetItem k with
        | Some _ -> true
        | None -> false

    // Slices
    // 2D
    member this.Item
        with get (k1f, k2f) =
            let keys1 = filterKeys k1f this.Keys1
            let keys2 = filterKeys k2f this.Keys2
            SMap2(keys1, keys2, this.Values)

    // 1D
    member this.Item
        with get (k1, k2f) =
            let keys2 = filterKeys k2f this.Keys2
            let getItem k = this.Values.GetItem (k1, k)
            let tryGetItem k = this.Values.TryGetItem (k1, k)
            let newStore = IStore.create getItem tryGetItem
            SMap (keys2, newStore)

    member this.Item
        with get (k1f, k2) =
            let keys1 = filterKeys k1f this.Keys1
            let getItem k = this.Values.GetItem (k, k2)
            let tryGetItem k = this.Values.TryGetItem (k, k2)
            let newStore = IStore.create getItem tryGetItem
            SMap (keys1, newStore)

    // 0D (aka GetItem)
    member this.Item
        with get(k1, k2) =
            this.Values.GetItem (k1, k2)

    // Operators
    static member inline (*) (coef, s:SMap2<_,_,_>) =
        let newValues = IStore.scale coef s.PossibleKeys s.Values
        SMap2(s.Keys1, s.Keys2, newValues)

    static member inline (*) (s:SMap2<_,_,_>, coef) =
        let newValues = IStore.scale coef s.PossibleKeys s.Values
        SMap2(s.Keys1, s.Keys2, newValues)

    static member inline (.*) (lhs:SMap2<_,_,_>, rhs:SMap2<_,_,_>) =
        let keys1 = Set.intersect lhs.Keys1 rhs.Keys1
        let keys2 = Set.intersect lhs.Keys2 rhs.Keys2
        let keySet = seq {for k1 in keys1 do for k2 in keys2 -> (k1, k2)}
        let rKeyBuilder = id
        let newValues = IStore.multiply keySet lhs.Values rKeyBuilder rhs.Values

        SMap2(keys1, keys2, newValues)

    static member inline (.*) (lhs:SMap2<_,_,_>, rhs:SMap<_,_>) =
        let keys1 = lhs.Keys1
        let keys2 = Set.intersect lhs.Keys2 rhs.Keys
        let keySet = seq {for k1 in keys1 do for k2 in keys2 -> (k1, k2)}
        let keyBuilder = fun (x, y) -> y
        let newValues = IStore.multiply keySet lhs.Values keyBuilder rhs.Values

        SMap2(keys1, keys2, newValues)

    static member inline (.*) (lhs:SMap<_,_>, rhs:SMap2<_,_,_>) =
        let keys1 = Set.intersect lhs.Keys rhs.Keys1
        let keys2 = rhs.Keys2
        let keySet = seq {for k1 in keys1 do for k2 in keys2 -> (k1, k2)}
        let keyBuilder = fun (x, y) -> x
        let newValues = IStore.multiply keySet rhs.Values keyBuilder lhs.Values

        SMap2(keys1, keys2, newValues)

    static member inline (+) (lhs:SMap2<_,_,_>, rhs:SMap2<_,_,_>) =
        let newKeys1 = lhs.Keys1 + rhs.Keys1
        let newKeys2 = lhs.Keys2 + rhs.Keys2
        let keySet = seq {for k1 in newKeys1 do for k2 in newKeys2 -> (k1, k2)}
        let newValues = IStore.mergeAdd keySet lhs.Values rhs.Values
        SMap2(newKeys1, newKeys2, newValues)

    static member inline Sum (m:SMap2<_,_,_>) =
        IStore.sum m.PossibleKeys m.Values

    static member inline Sum (m:SMap2<_,_,Flips.Types.Decision>) =
        IStore.sumDecisions m.PossibleKeys m.Values

    static member inline Sum (m:SMap2<_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
        IStore.sumDecisionsWithUnits m.PossibleKeys m.Values


module SMap2 =

    let ofSeq m =
        m |> SMap2

    let toSeq (m:SMap2<_,_,_>) =
        IStore.toSeq m.PossibleKeys m.Values

    let ofMap m =
        m |> SMap2

    let toMap (m:SMap2<_,_,_>) =
        m |> toSeq |> Map.ofSeq

    let ofList m =
        m |> Seq.ofList |> SMap2

    let toList (m:SMap2<_,_,_>) =
        m |> toSeq |> List.ofSeq

    let ofArray m =
        m |> Seq.ofArray |> SMap2

    let toArray (m:SMap2<_,_,_>) =
        m |> toSeq |> Array.ofSeq

    let containsKey k (m:SMap2<_,_,_>) =
        m.ContainsKey k

    let inline reKey f m =
        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq


//type SMap3<'Key1, 'Key2, 'Key3, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Value : equality> (m:Map<('Key1 * 'Key2 * 'Key3),'Value>) =

//    member this.Values = m

//    override this.ToString() =
//        sprintf "SMap3 %O" this.Values

//    override this.Equals(obj) =
//        match obj with
//        | :? SMap3<'Key1, 'Key2, 'Key3, 'Value> as s -> this.Values = s.Values
//        | _ -> false

//    override this.GetHashCode () =
//        hash this.Values

//    member this.ContainsKey k =
//        Map.containsKey k this.Values

//    member this.AsMap =
//        this.Values

//    // Filter Values
//    member private this.FilterValues k1f k2f k3f =
//        let k1Filter = SliceFilterBuilder k1f
//        let k2Filter = SliceFilterBuilder k2f
//        let k3Filter = SliceFilterBuilder k3f
        
//        this.Values
//        |> Map.filter (fun (k1, k2, k3) _ -> k1Filter k1 && k2Filter k2 && k3Filter k3)
//        |> Map.toSeq

//    // Slices
//    // 3D
//    member this.Item
//        with get (k1f, k2f, k3f) =
//            this.FilterValues k1f k2f k3f |> Map.ofSeq |> SMap3

//    // 2D
//    member this.Item
//        with get (k1, k2f, k3f) =
//            this.FilterValues (Equals k1) k2f k3f
//            |> Seq.map (fun ((k1, k2, k3), v) -> (k2, k3), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2, k3f) =
//            this.FilterValues k1f (Equals k2) k3f
//            |> Seq.map (fun ((k1, k2, k3), v) -> (k1, k3), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2f, k3) =
//            this.FilterValues k1f k2f (Equals k3)
//            |> Seq.map (fun ((k1, k2, k3), v) -> (k1, k2), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    // 1D
//    member this.Item
//        with get (k1, k2, k3f) =
//            this.FilterValues (Equals k1) (Equals k2) k3f
//            |> Seq.map (fun ((k1, k2, k3), v) -> k3, v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1, k2f, k3) =
//            this.FilterValues (Equals k1) k2f (Equals k3)
//            |> Seq.map (fun ((k1, k2, k3), v) -> k2, v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1f, k2, k3) =
//            this.FilterValues k1f (Equals k2) (Equals k3)
//            |> Seq.map (fun ((k1, k2, k3), v) -> k1, v) 
//            |> Map.ofSeq 
//            |> SMap

//    // 0D (aka GetItem)
//    member this.Item
//        with get(k1, k2, k3) =
//            this.Values.[(k1, k2, k3)] 

//    // Operators
//    static member inline (*) (lhs, rhs:SMap3<_,_,_,_>) =
//        rhs.Values
//        |> Map.map (fun k v -> lhs * v)
//        |> SMap3

//    static member inline (*) (lhs:SMap3<_,_,_,_>, rhs) =
//        lhs.Values
//        |> Map.map (fun k v -> rhs * v)
//        |> SMap3

//    static member inline (.*) (lhs:SMap3<_,_,_,_>, rhs:SMap3<_,_,_,_>) =
//        lhs.Values
//        |> Map.filter (fun k _ -> rhs.ContainsKey k)
//        |> Map.map (fun (k1, k2, k3) v -> v * rhs.[k1, k2, k3])
//        |> SMap3

//    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap2<_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey (k2, k3))
//        |> Map.map (fun (k1, k2, k3) v -> v * b.[k2, k3])
//        |> SMap3

//    static member inline (.*) (b:SMap2<_,_,_>, a:SMap3<_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey (k1, k2))
//        |> Map.map (fun (k1, k2, k3) v -> v * b.[k1, k2])
//        |> SMap3

//    static member inline (.*) (a:SMap3<_,_,_,_>, b:SMap<_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey k3)
//        |> Map.map (fun (k1, k2, k3) v -> v * b.[k3])
//        |> SMap3

//    static member inline (.*) (b:SMap<_,_>, a:SMap3<_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3) _ -> b.ContainsKey k1)
//        |> Map.map (fun (k1, k2, k3) v -> v * b.[k1])
//        |> SMap3

//    static member inline (+) (lhs:SMap3<_,_,_,_>, rhs:SMap3<_,_,_,_>) =
//        match Map.count lhs.Values > Map.count rhs.Values with
//        | true ->  mergeAddition lhs.Values rhs.Values
//        | false -> mergeAddition rhs.Values lhs.Values
//        |> SMap3

//    static member inline Sum (m:SMap3<_,_,_,_>) =
//        m.Values |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap3<_,_,_,Flips.Types.Decision>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap3<_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


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


//type SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Value : equality> (m:Map<('Key1 * 'Key2 * 'Key3 * 'Key4),'Value>) =

//    member this.Values = m

//    override this.ToString() =
//        sprintf "SMap4 %O" this.Values

//    override this.Equals(obj) =
//        match obj with
//        | :? SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value> as s -> this.Values = s.Values
//        | _ -> false

//    override this.GetHashCode () =
//        hash this.Values

//    member this.ContainsKey k =
//        Map.containsKey k this.Values

//    member this.AsMap =
//        this.Values

//    // Filter Values
//    member private this.FilterValues k1f k2f k3f k4f =
//        let k1Filter = SliceFilterBuilder k1f
//        let k2Filter = SliceFilterBuilder k2f
//        let k3Filter = SliceFilterBuilder k3f
//        let k4Filter = SliceFilterBuilder k4f
        
//        this.Values
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
//            this.Values.[k1, k2, k3, k4] 

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


//type SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Key5 : comparison and 'Value : equality> (m:Map<('Key1 * 'Key2 * 'Key3 * 'Key4 * 'Key5),'Value>) =

//    member this.Values = m

//    override this.ToString() =
//        sprintf "SMap5 %O" this.Values

//    override this.Equals(obj) =
//        match obj with
//        | :? SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value> as s -> this.Values = s.Values
//        | _ -> false

//    override this.GetHashCode () =
//        hash this.Values

//    member this.ContainsKey k =
//        Map.containsKey k this.Values

//    member this.AsMap =
//        this.Values

//    // Filter Values
//    member private this.FilterValues k1f k2f k3f k4f k5f =
//        let k1Filter = SliceFilterBuilder k1f
//        let k2Filter = SliceFilterBuilder k2f
//        let k3Filter = SliceFilterBuilder k3f
//        let k4Filter = SliceFilterBuilder k4f
//        let k5Filter = SliceFilterBuilder k5f
        
//        this.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> k1Filter k1 && k2Filter k2 && k3Filter k3 && k4Filter k4 && k5Filter k5)
//        |> Map.toSeq

//    // Slices
//    // 5D
//    member this.Item
//        with get (k1f, k2f, k3f, k4f, k5f) =
//            this.FilterValues k1f k2f k3f k4f k5f |> Map.ofSeq |> SMap5

//    // 4D
//    member this.Item
//        with get (k1, k2f, k3f, k4f, k5f) =
//            this.FilterValues (Equals k1) k2f k3f k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2, k3, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap4

//    member this.Item
//        with get (k1f, k2, k3f, k4f, k5f) =
//            this.FilterValues k1f (Equals k2) k3f k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k3, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap4

//    member this.Item
//        with get (k1f, k2f, k3, k4f, k5f) =
//            this.FilterValues k1f k2f (Equals k3) k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap4

//    member this.Item
//        with get (k1f, k2f, k3f, k4, k5f) =
//            this.FilterValues k1f k2f k3f (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k3, k5), v) 
//            |> Map.ofSeq 
//            |> SMap4

//    member this.Item
//        with get (k1f, k2f, k3f, k4f, k5) =
//            this.FilterValues k1f k2f k3f k4f (Equals k5) 
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap4


//    // 3D
//    member this.Item
//        with get (k1, k2, k3f, k4f, k5f) =
//            this.FilterValues (Equals k1) (Equals k2) k3f k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k3, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1, k2f, k3, k4f, k5f) =
//            this.FilterValues (Equals k1) k2f (Equals k3) k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1, k2f, k3f, k4, k5f) =
//            this.FilterValues (Equals k1) k2f k3f (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2, k3, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1, k2f, k3f, k4f, k5) =
//            this.FilterValues (Equals k1) k2f k3f k4f (Equals k5) 
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2, k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2, k3, k4f, k5f) =
//            this.FilterValues k1f (Equals k2) (Equals k3) k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2, k3f, k4, k5f) =
//            this.FilterValues k1f (Equals k2) k3f (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k3, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2, k3f, k4f, k5) =
//            this.FilterValues k1f (Equals k2) k3f k4f (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2f, k3, k4, k5f) =
//            this.FilterValues k1f k2f (Equals k3) (Equals k4) k5f 
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k5), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2f, k3, k4f, k5) =
//            this.FilterValues k1f k2f (Equals k3) k4f (Equals k5)  
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k4), v) 
//            |> Map.ofSeq 
//            |> SMap3

//    member this.Item
//        with get (k1f, k2f, k3f, k4, k5) =
//            this.FilterValues k1f k2f k3f (Equals k4) (Equals k5)  
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2, k3), v) 
//            |> Map.ofSeq 
//            |> SMap3


//    // 2D
//    member this.Item
//        with get (k1, k2, k3, k4f, k5f) =
//            this.FilterValues (Equals k1) (Equals k2) (Equals k3) k4f k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k4, k5), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1, k2, k3f, k4, k5f) =
//            this.FilterValues (Equals k1) (Equals k2) k3f (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k3, k5), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1, k2, k3f, k4f, k5) =
//            this.FilterValues (Equals k1) (Equals k2) k3f k4f (Equals k5) 
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k3, k4), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2, k3, k4, k5f) =
//            this.FilterValues k1f (Equals k2) (Equals k3) (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k5), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2, k3, k4f, k5) =
//            this.FilterValues k1f (Equals k2) (Equals k3) k4f (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k4), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    member this.Item
//        with get (k1f, k2f, k3, k4, k5) =
//            this.FilterValues k1f k2f (Equals k3) (Equals k4) (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1, k2), v) 
//            |> Map.ofSeq 
//            |> SMap2

//    // 1D
//    member this.Item
//        with get (k1, k2, k3, k4, k5f) =
//            this.FilterValues (Equals k1) (Equals k2) (Equals k3) (Equals k4) k5f
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k5), v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1, k2, k3, k4f, k5) =
//            this.FilterValues (Equals k1) (Equals k2) (Equals k3) k4f (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k4), v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1, k2, k3f, k4, k5) =
//            this.FilterValues (Equals k1) (Equals k2) k3f (Equals k4) (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k3), v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1, k2f, k3, k4, k5) =
//            this.FilterValues (Equals k1) k2f (Equals k3) (Equals k4) (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k2), v) 
//            |> Map.ofSeq 
//            |> SMap

//    member this.Item
//        with get (k1f, k2, k3, k4, k5) =
//            this.FilterValues k1f (Equals k2) (Equals k3) (Equals k4) (Equals k5)
//            |> Seq.map (fun ((k1, k2, k3, k4, k5), v) -> (k1), v) 
//            |> Map.ofSeq 
//            |> SMap

//    // 0D (aka GetItem)
//    member this.Item
//        with get(k1, k2, k3, k4, k5) =
//            this.Values.[k1, k2, k3, k4, k5] 

//    // Operators
//    static member inline (*) (lhs, rhs:SMap5<_,_,_,_,_,_>) =
//        rhs.Values
//        |> Map.map (fun k v -> lhs * v)
//        |> SMap5

//    static member inline (*) (lhs:SMap5<_,_,_,_,_,_>, rhs) =
//        lhs.Values
//        |> Map.map (fun k v -> rhs * v)
//        |> SMap5

//    static member inline (.*) (lhs:SMap5<_,_,_,_,_,_>, rhs:SMap5<_,_,_,_,_,_>) =
//        lhs.Values
//        |> Map.filter (fun k _ -> rhs.ContainsKey k)
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * rhs.[k1, k2, k3, k4, k5])
//        |> SMap5

//    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap4<_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k2, k3, k4, k5))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k2, k3, k4, k5])
//        |> SMap5

//    static member inline (.*) (b:SMap4<_,_,_,_,_>, a:SMap5<_,_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k1, k2, k3, k4))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k1, k2, k3, k4])
//        |> SMap5

//    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap3<_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k3, k4, k5))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k3, k4, k5])
//        |> SMap5

//    static member inline (.*) (b:SMap3<_,_,_,_>, a:SMap5<_,_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k1, k2, k3))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k1, k2, k3])
//        |> SMap5

//    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap2<_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k4, k5))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k4, k5])
//        |> SMap5

//    static member inline (.*) (b:SMap2<_,_,_>, a:SMap5<_,_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey (k1, k2))
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k1, k2])
//        |> SMap5

//    static member inline (.*) (a:SMap5<_,_,_,_,_,_>, b:SMap<_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey k5)
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k5])
//        |> SMap5

//    static member inline (.*) (b:SMap<_,_>, a:SMap5<_,_,_,_,_,_>) =
//        a.Values
//        |> Map.filter (fun (k1, k2, k3, k4, k5) _ -> b.ContainsKey k1)
//        |> Map.map (fun (k1, k2, k3, k4, k5) v -> v * b.[k1])
//        |> SMap5


//    static member inline (+) (lhs:SMap5<_,_,_,_,_,_>, rhs:SMap5<_,_,_,_,_,_>) =
//        match Map.count lhs.Values > Map.count rhs.Values with
//        | true ->  mergeAddition lhs.Values rhs.Values
//        | false -> mergeAddition rhs.Values lhs.Values
//        |> SMap5

//    static member inline Sum (m:SMap5<_,_,_,_,_,_>) =
//        m.Values |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap5<_,_,_,_,_,Flips.Types.Decision>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd

//    static member inline Sum (m:SMap5<_,_,_,_,_,Flips.UnitsOfMeasure.Types.Decision<_>>) =
//        m.Values |> Map.map (fun _ d -> 1.0 * d) |> Map.toSeq |> Seq.sumBy snd


//module SMap5 =

//    let ofMap m =
//        m |> SMap5

//    let toMap (m:SMap5<_,_,_,_,_,_>) =
//        m.Values

//    let ofList m =
//        m |> Map.ofList |> SMap5

//    let toList (m:SMap5<_,_,_,_,_,_>) =
//        m.Values |> Map.toList

//    let ofSeq m =
//        m |> Map.ofSeq |> SMap5

//    let toSeq (m:SMap5<_,_,_,_,_,_>) =
//        m.Values |> Map.toSeq

//    let ofArray m =
//        m |> Map.ofArray |> SMap5

//    let toArray (m:SMap5<_,_,_,_,_,_>) =
//        m.Values |> Map.toArray

//    let containsKey k (m:SMap5<_,_,_,_,_,_>) =
//        Map.containsKey k m.Values

//    let reKey f m =
//        m |> toSeq |> Seq.map (fun (k, v) -> (f k), v) |> ofSeq