#load "../../tools/doc.samples.load.fsx"
#r "../../bin/netstandard2.0/Flips.Solver.Legacy.dll"
#r "../../bin/netstandard2.0/Google.OrTools.dll"
#r "../../bin/netstandard2.0/NativeLibraryLoader.dll"
//#r "nuget: Google.OrTools"
//#r "nuget: NativeLibraryLoader"
open NativeLibraryLoader
new NativeLibrary(__SOURCE_DIRECTORY__ + @"/../../bin/netstandard2.0/libz3.dll")
new NativeLibrary(__SOURCE_DIRECTORY__ + @"/../../bin/netstandard2.0/google-ortools-native.dll")

let model = Flips.Docs.Samples.sampleProblem()
open Flips
open Flips.Legacy.Types
let settings =
    { SolverType   = SolverType.GLOP
      MaxDuration  = 10000000L
      WriteLPFile  = None
      WriteMPSFile = None }
let result = Solver.solve settings model

// val result : SolveResult =
//   Optimal
//     { DecisionResults =
//                        map
//                          [({ Name = DecisionName "x1"
//                              Type = Continuous (0.0, infinity) }, 7.0);
//                           ({ Name = DecisionName "x2"
//                              Type = Continuous (0.0, infinity) }, 5.0)]
//       ObjectiveResult = 29.0 }val result : SolveResult =
