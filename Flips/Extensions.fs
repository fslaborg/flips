module Flips.Extensions

// This is necessary since F# does not let you define additional
// operators in type extensions
let inline (.*) (lhs:Map<_,_>) (rhs:Map<_,_>) =
    lhs
    |> Map.filter (fun k _ -> Map.containsKey k rhs)
    |> Map.map (fun k v -> v * rhs.[k])


type Map<'Key, 'Value when 'Key : comparison> with
    member this.Item
        with get(values) =
            this |> Map.filter (fun k _ -> Set.contains k values)

    member this.GetSlice (lowerBound : 'Key option, upperBound : 'Key option) =
        this
        |> Map.filter (fun k _ -> match lowerBound, upperBound with
                                  | Some lb, Some ub -> k >= lb && k <= ub
                                  | Some lb, None -> k >= lb
                                  | None, Some ub -> k <= ub
                                  | None, None -> true)

module Map =

    let inline sum m =
        m
        |> Map.toSeq
        |> Seq.sumBy snd


// Declared here so it can be used by any of the MapXD types
let inline private getChecker lb ub =
    match lb, ub with
    | Some lb, Some ub -> fun x -> x >= lb && x <= ub
    | Some lb, None -> fun x -> x >= lb
    | None, Some ub -> fun x -> x <= ub
    | None, None -> fun _ -> true


type Map2D<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison> (m:Map<('Key1 * 'Key2),'Value>) =

    member private this.Values = m

    override this.ToString() =
        sprintf "Map2D %O" this.Values

    member this.ContainsKey k =
        Map.containsKey k this.Values

    member this.Item
        with get(k) =
            this.Values.[k] 

    member this.GetSlice (sk1 : _,
                          lb2 : 'Key2 option,
                          ub2 : 'Key2 option) = 
                            let k2Check = getChecker lb2 ub2
                            this.Values
                            |> Map.filter (fun (k1, k2) _ -> k1 = sk1 && k2Check k2)
                            |> Map2D

    member this.GetSlice (lb1 : 'Key1 option,
                          ub1 : 'Key1 option,
                          sk2 : _) =
                            let k1Check = getChecker lb1 ub1
                            this.Values
                            |> Map.filter (fun (k1, k2) _ -> k1Check k1 && k2 = sk2)

    member this.GetSlice (lb1 : 'Key1 option,
                          ub1 : 'Key1 option,
                          lb2 : 'Key2 option,
                          ub2 : 'Key2 option) =

        let key1Check = getChecker lb1 ub1
        let key2Check = getChecker lb2 ub2
        
        m |> Map.filter (fun (k1, k2) _ -> (key1Check k1) && (key2Check k2))

    static member inline (.*) (lhs:Map2D<_,_,_>, rhs:Map<_,_>) =
        lhs.Values
        |> Map.filter (fun (_, k2) _ -> Map.containsKey k2 rhs)
        |> Map.map (fun (_, k2) v -> v * rhs.[k2])

    static member inline (.*) (lhs:Map<_,_>, rhs:Map2D<_,_,_>) =
        rhs.Values
        |> Map.filter (fun (_, k2) _ -> Map.containsKey k2 lhs)
        |> Map.map (fun (_, k2) v -> v * lhs.[k2])

    static member inline (.*) (lhs:Map2D<_,_,_>, rhs:Map2D<_,_,_>) =
        lhs.Values
        |> Map.filter (fun (k1, k2) _ -> rhs.ContainsKey (k1, k2))
        |> Map.map (fun (k1, k2) v -> v * rhs.[(k1, k2)])

    static member toSeq (m:Map2D<_,_,_>) =
        m.Values
        |> Map.toSeq

    static member inline sum (m:Map2D<_,_,_>) =
        m.Values
        |> Map.toSeq
        |> Seq.sumBy snd


module Map2D =

    let ofList l =
        l |> Map.ofList |> Map2D

