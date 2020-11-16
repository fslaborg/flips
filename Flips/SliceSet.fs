namespace Flips.SliceMap

open System
open System.Collections.Generic


[<NoComparison;NoEquality>]
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

[<RequireQualifiedAccess>]
type RankResult =
    | Exact of int
    | Closest of int
    | Empty

[<NoComparison>]
type SliceSet<[<EqualityConditionalOn>]'T when 'T : comparison>(comparer:IComparer<'T>, values:Memory<'T>) =
    let comparer = comparer
    let values = values

    static let empty : SliceSet<'T> =
        let comparer = LanguagePrimitives.FastGenericComparer<'T>
        let m = [||]
        SliceSet<'T>(comparer, m.AsMemory())


    let findIndex (values:Memory<_>) startingLowerBound lookup : Option<int> =
        let mutable lowerBound = startingLowerBound
        let mutable upperBound = values.Length - 1
        let mutable idx = 0
        let mutable result = None

        if values.Length > 0 then

          while lowerBound <= upperBound && result = None do
              idx <- lowerBound + (upperBound - lowerBound) / 2 // int math defaults to Floor for rounding
              let x = comparer.Compare(values.Span.[idx], lookup)
              if x < 0 then
                  lowerBound <- idx + 1
              elif x > 0 then
                  upperBound <- idx - 1
              else
                  result <- Some idx

        result


    let rank (values:Memory<_>) lookup : int =
        let mutable lowerBound = 0
        let mutable upperBound = values.Length
        let mutable idx = 0

        while (lowerBound < upperBound) do
            idx <- lowerBound + (upperBound - lowerBound) / 2 // int math defaults to Floor for rounding
            let x = comparer.Compare(values.Span.[idx], lookup)
            if x < 0 then
                lowerBound <- idx + 1
            else
                upperBound <- idx

        lowerBound


    new(values:seq<'T>) =
        let comparer = LanguagePrimitives.FastGenericComparer<'T>
        let v = values |> Seq.distinct |> Seq.toArray |> Array.sort
        SliceSet(comparer, v.AsMemory<'T>())

    member _.Item
        with get (idx) =
            values.Span.[idx]

    override this.Equals(obj) =
        match obj with
        | :? SliceSet<'T> as other ->
            let mutable result = true
            let mutable idx = 0
            if this.Count <> other.Count then
                result <- false

            while result && (idx < this.Count) do

                if this.[idx] <> other.[idx] then
                    result <- false

                idx <- idx + 1

            result

        | _ -> false

    override _.GetHashCode () =
        hash values

    interface IEnumerable<'T> with
        member _.GetEnumerator(): IEnumerator<'T> = 
            let s = seq { for idx in 0..values.Length-1 -> values.Span.[idx] }
            s.GetEnumerator()

    interface System.Collections.IEnumerable with
        member _.GetEnumerator(): Collections.IEnumerator = 
            let s = seq { for idx in 0..values.Length-1 -> values.Span.[idx] }
            s.GetEnumerator() :> Collections.IEnumerator

    member internal _.Comparer = comparer
    member internal _.Values = values

    member _.GreaterThan x =

        if values.Length = 0 then
            empty
        else
            let r = rank values x
            if r = values.Length then
                empty
            elif (comparer.Compare (values.Span.[r], x) = 0) then
                SliceSet (comparer, values.Slice(r + 1))
            else
                SliceSet (comparer, values.Slice(r))


    member _.GreaterOrEqual x =

        if values.Length = 0 then
            empty
        else
            let r = rank values x
            if r < values.Length then
                SliceSet (comparer, values.Slice(r))
            else
                empty


    member _.LessThan x =

        let r = rank values x
        SliceSet (comparer, values.Slice(0, r))


    member _.LessOrEqual x =

        if values.Length = 0 then
            empty
        else
            let r = rank values x
            if (r < values.Length) && (comparer.Compare (values.Span.[r], x) = 0) then
                SliceSet (comparer, values.Slice(0, r + 1))
            else
                SliceSet (comparer, values.Slice(0, r))


    member _.Between lowerBound upperBound =

        // If there are no values, return empty
        if values.Length = 0 then 
            empty
        // If there is only 1 element, check if it is within the bound
        elif values.Length = 1 then
            if values.Span.[0] >= lowerBound && values.Span.[0] <= upperBound then
                SliceSet (comparer, values)
            else
                empty
        // At this point we know there are at least 2 elements in values
        else 
            let lowerRank = rank values lowerBound
            let upperRank = rank values upperBound

            let lowerIdx = 
              match (comparer.Compare (values.Span.[lowerRank], lowerBound) = 0) with
              | true -> lowerRank
              | false -> lowerRank + 1

            let upperIdx =
                match (comparer.Compare (values.Span.[upperRank], upperBound) = 0) with
                | true -> upperRank
                | false -> upperRank - 1

            SliceSet (comparer, values.Slice(lowerIdx, upperIdx - lowerIdx + 1))
                  


    member _.Intersect (b:SliceSet<'T>) =

        let intersectAux (small:Memory<'T>) (large:Memory<'T>) =
            let newValues = Array.zeroCreate(small.Length)

            let mutable smallIdx = 0
            let mutable largeIdx = 0
            let mutable outIdx = 0

            while (smallIdx < small.Length && largeIdx < large.Length) do
                let c = comparer.Compare(small.Span.[smallIdx], large.Span.[largeIdx])

                if c = 0 then
                    newValues.[outIdx] <- small.Span.[smallIdx]
                    smallIdx <- smallIdx + 1
                    largeIdx <- largeIdx + 1
                    outIdx <- outIdx + 1
                elif c < 0 then
                    smallIdx <- smallIdx + 1
                else
                    largeIdx <- largeIdx + 1

            SliceSet(comparer, newValues.AsMemory().Slice(0, outIdx))


        if values.Length < b.Values.Length then
          intersectAux values b.Values
        else
          intersectAux b.Values values


    member _.Union (b:SliceSet<'T>) =
        let newValues = Array.zeroCreate(values.Length + b.Values.Length)

        let mutable aIdx = 0
        let mutable bIdx = 0
        let mutable outIdx = 0

        while (aIdx < values.Length && bIdx < b.Values.Length) do
            
            let c = comparer.Compare(values.Span.[aIdx], b.Values.Span.[bIdx])

            if c < 0 then
                newValues.[outIdx] <- values.Span.[aIdx]
                aIdx <- aIdx + 1
                outIdx <- outIdx + 1
            elif c = 0 then
                newValues.[outIdx] <- values.Span.[aIdx]
                aIdx <- aIdx + 1
                bIdx <- bIdx + 1
                outIdx <- outIdx + 1
            else
                newValues.[outIdx] <- b.Values.Span.[bIdx]
                bIdx <- bIdx + 1
                outIdx <- outIdx + 1

        while aIdx < values.Length do
            newValues.[outIdx] <- values.Span.[aIdx]
            aIdx <- aIdx + 1
            outIdx <- outIdx + 1

        while bIdx < b.Values.Length do
            newValues.[outIdx] <- b.Values.Span.[bIdx]
            bIdx <- bIdx + 1
            outIdx <- outIdx + 1

        SliceSet(comparer, newValues.AsMemory(0, outIdx))


    member _.Minus (b:SliceSet<'T>) =
        let newValues = Array.zeroCreate(values.Length)

        let mutable aIdx = 0
        let mutable bIdx = 0
        let mutable outIdx = 0

        while (aIdx < values.Length && bIdx < b.Values.Length) do
            
            let c = comparer.Compare(values.Span.[aIdx], b.Values.Span.[bIdx])

            if c < 0 then
                newValues.[outIdx] <- values.Span.[aIdx]
                outIdx <- outIdx + 1
                aIdx <- aIdx + 1
            elif c = 0 then
                aIdx <- aIdx + 1
                bIdx <- bIdx + 1
            else
                bIdx <- bIdx + 1

        while aIdx < values.Length do
            newValues.[outIdx] <- values.Span.[aIdx]
            aIdx <- aIdx + 1
            outIdx <- outIdx + 1

        SliceSet(comparer, newValues.AsMemory(0, outIdx))


    member _.Filter f =
        let newValues = Array.zeroCreate(values.Length)

        let mutable idx = 0
        let mutable outIdx = 0

        while idx < values.Length do
            if f values.Span.[idx] then
                newValues.[outIdx] <- values.Span.[idx]
                outIdx <- outIdx + 1
            
            idx <- idx + 1

        SliceSet(comparer, newValues.AsMemory(0, outIdx))


    member _.Contains x =
        let result = findIndex values 0 x
        match result with
        | Some idx -> 
          let c = comparer.Compare(values.Span.[idx], x)
          if c = 0 then
              true
          else
              false
        | None -> false


    member _.Count =
        values.Length


    static member (+) (a:SliceSet<'T>, b:SliceSet<'T>) =
        a.Union(b)


    static member (-) (a:SliceSet<'T>, b:SliceSet<'T>) =
        a.Minus(b)


[<RequireQualifiedAccess>]
module SliceSet =

    let intersect (a:SliceSet<_>) b =
        a.Intersect b

    let toSeq (a:SliceSet<_>) =
        seq { for i in 0..a.Count - 1 -> a.Values.Span.[i] }

    let toList (a:SliceSet<_>) =
        a |> toSeq |> List.ofSeq

    let union (a:SliceSet<_>) (b:SliceSet<_>) =
        a.Union b

    let slice (f:SliceType<_>) (keys:SliceSet<_>) =
        match f with
        | All -> keys
        | Equals k -> match keys.Contains k with | true -> SliceSet [k] | false -> SliceSet []
        | GreaterThan k -> keys.GreaterThan k
        | GreaterOrEqual k -> keys.GreaterOrEqual k
        | LessThan k -> keys.LessThan k
        | LessOrEqual k -> keys.LessOrEqual k
        | Between (lowerBound, upperBound) -> keys.Between lowerBound upperBound
        | In set -> keys.Intersect (SliceSet set)
        | NotIn set -> keys.Minus (SliceSet set)
        | Where f -> keys.Filter f