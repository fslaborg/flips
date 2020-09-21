namespace Flips.SliceMap

[<AutoOpen>]
module Sum =

    let inline sum< ^a, ^b when ^a: (static member Sum: ^a -> ^b)> (a: ^a) = 
        ((^a) : (static member Sum: ^a -> ^b) a)

    let inline sumAll< ^a, ^b when ^a: (static member Sum: ^a -> ^b) 
                              and ^a: (static member (+): ^a * ^a -> ^a)
                              and ^a: (static member Zero: ^a)> (k1: ^a seq) = 
        let r = Seq.sum k1
        ((^a) : (static member Sum: ^a -> ^b) r)