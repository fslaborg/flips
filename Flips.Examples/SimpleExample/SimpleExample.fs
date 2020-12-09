module Flips.Examples.SimpleExample

open Flips

let solve settings =

    let result = Solver.solve settings SampleProblems.SimpleExample.model
    printfn "%A" result

