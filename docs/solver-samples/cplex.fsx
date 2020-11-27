#load "../../tools/doc.samples.load.fsx"
#r "../../bin/netstandard2.0/Flips.Solver.Cplex.dll"
#r "../../bin/netstandard2.0/ILOG.Concert.dll"
#r "../../bin/netstandard2.0/ILOG.CPLEX.dll"
open Flips.Solver.Cplex.Internals

let model = Flips.Docs.Samples.sampleProblem()

let state =
    CplexSolver.ICplexSolverState.create()
    |> CplexSolver.ICplexSolverState.removeOption (
            function
            | CplexSolver.DoNotReuseState -> true // removing it allows to reuse same state object for several calls to solve
            | _ -> false
    )
    |> CplexSolver.ICplexSolverState.setOption (CplexSolver.WriteToFile @"c:\tmp\flips.sample.cplex.lp") 
    |> CplexSolver.ICplexSolverState.setOption (CplexSolver.WriteToFile @"c:\tmp\flips.sample.cplex.mps")

let cplexResults = CplexSolver.Primitives.solve model state

printfn "%A" cplexResults
// > 
// Ok
//   { DecisionResults =
//                      map
//                        [({ Name = DecisionName "x1"
//                            Type = Continuous (0.0, infinity) }, 7.0);
//                         ({ Name = DecisionName "x2"
//                            Type = Continuous (0.0, infinity) }, 5.0)]
//     ObjectiveResult = 29.0 }
// val it : unit = ()
