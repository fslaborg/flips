namespace Flips.KeySet

open System
open System.Collections.Generic

type KeySet<[<EqualityConditionalOn>]'T when 'T : comparison>(comparer:IComparer<'T>, values:Memory<'T>) =
    let comparer = comparer
    let values = values

    static let empty : KeySet<'T> =
        let comparer = LanguagePrimitives.FastGenericComparer<'T>
        let m = [||]
        KeySet<'T>(comparer, m.AsMemory())

    let findIndexOf (comparer:IComparer<'T>) startingLowerBound x (values:Memory<'T>) =
        let mutable lowerBound = startingLowerBound
        let mutable upperBound = values.Length - 1
        let mutable idx = (lowerBound + upperBound) / 2

        while lowerBound <= upperBound do
            let x = comparer.Compare(values.Span.[idx], x)
            if x <= 0 then
                lowerBound <- idx + 1
                idx <- (lowerBound + upperBound) / 2
            else
                upperBound <- idx - 1
                idx <- (lowerBound + upperBound) / 2

        idx

    new(values:Set<'T>) =
        let comparer = LanguagePrimitives.FastGenericComparer<'T>
        let v = Set.toArray values
        KeySet(comparer, v.AsMemory<'T>())

    member internal _.Comparer = comparer
    member internal _.Values = values

    member _.GreaterThan x =

        if values.IsEmpty then
            empty
        elif values.Length = 1 then
            if values.Span.[0] > x then
                KeySet(comparer, values)
            else
                empty
        else
            let idx = findIndexOf comparer 0 x values
            KeySet (comparer, values.Slice(idx + 1))

    member _.GreaterOrEqual x =
        if values.IsEmpty then
            empty
        elif values.Length = 1 then
            if values.Span.[0] > x then
                KeySet(comparer, values)
            else
                empty
        else
            let idx = findIndexOf comparer 0 x values
            KeySet (comparer, values.Slice(idx))

    member _.LessThan x =

        if values.IsEmpty then
            empty
        elif values.Length = 1 then
            if values.Span.[0] > x then
                KeySet(comparer, values)
            else
                empty
        else
            let idx = findIndexOf comparer 0 x values
            KeySet (comparer, values.Slice(0, idx))

    member _.LessOrEqual x =

        if values.IsEmpty then
            empty
        elif values.Length = 1 then
            if values.Span.[0] > x then
                KeySet(comparer, values)
            else
                empty
        else
            let idx = findIndexOf comparer 0 x values
            KeySet (comparer, values.Slice(0, idx + 1))

    member _.Intersect (b:KeySet<'T>) =
        let intersectAux (small:Memory<'T>) (large:Memory<'T>) =

          let newValues = Array.zeroCreate(small.Length)

          let mutable smallIdx = 0
          let mutable largeLowerIdx = 0
          let mutable outIdx = 0

          while (smallIdx < small.Length) do
              largeLowerIdx <- findIndexOf comparer largeLowerIdx (small.Span.[smallIdx]) large

              let c = comparer.Compare(small.Span.[smallIdx], large.Span.[largeLowerIdx])

              if c = 0 then
                  newValues.[outIdx] <- small.Span.[smallIdx]
                  outIdx <- outIdx + 1

              smallIdx <- smallIdx + 1

          KeySet(comparer, newValues.AsMemory().Slice(0, outIdx))

        if values.Length < b.Values.Length then
          intersectAux values b.Values
        else
          intersectAux b.Values values

    member _.Add (b:KeySet<'T>) =
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

        KeySet(comparer, newValues.AsMemory(0, outIdx))

    member _.Minus (b:KeySet<'T>) =
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

        KeySet(comparer, newValues.AsMemory(0, outIdx))

    member _.Filter f =
        let newValues = Array.zeroCreate(values.Length)

        let mutable idx = 0
        let mutable outIdx = 0

        while idx < values.Length do
            if f values.Span.[idx] then
                newValues.[outIdx] <- values.Span.[idx]
                outIdx <- outIdx + 1
            
            idx <- idx + 1

        KeySet(comparer, newValues.AsMemory(0, outIdx))

