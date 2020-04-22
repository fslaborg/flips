module Flips.Extensions


let inline (.*) (lhs:Map<_,_>) (rhs:Map<_,_>) =
    lhs
    |> Map.filter (fun k _ -> Map.containsKey k rhs)
    |> Map.map (fun k v -> v * rhs.[k])


let inline sum m =
    m
    |> Map.toSeq
    |> Seq.sumBy snd


type Map<'Key, 'Value when 'Key : comparison> with
    member this.Item
        with get(values) =
            this |> Map.filter (fun k _ -> Set.contains k values)