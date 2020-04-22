module Flips.Extensions

let inline (.*) (lhs, rhs) =
    lhs
    |> Map.filter (fun k _ -> Map.containsKey k rhs)
    |> Map.map (fun k v -> v * rhs.[k])


let inline sum m =
    m
    |> Map.toSeq
    |> Seq.sumBy snd