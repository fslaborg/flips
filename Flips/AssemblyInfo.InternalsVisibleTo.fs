module AssemblyInfo.InternalsVisibleTo

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Flips.Solver")>]
[<assembly: InternalsVisibleTo("Flips.Solver.CBC")>]
[<assembly: InternalsVisibleTo("Flips.Solver.Cplex")>]
[<assembly: InternalsVisibleTo("Flips.Solver.FlipsVersion2")>]
[<assembly: InternalsVisibleTo("Flips.Solver.OPTANO")>]

do()

