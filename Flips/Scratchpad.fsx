//#I @"C:\Users\matth\.nuget\packages\"

//#r @"C:\Users\matth\.nuget\packages\google.ortools\7.5.7466\lib\netstandard2.1\Google.OrTools.dll"
//#r @"C:\Users\matth\.nuget\packages\google.ortools.runtime.win-x64\7.5.7466\runtimes\win-x64\native\google-ortools-native.dll"

#load "Types.fs"
#load "Domain.fs"
#load "UnitsOfMeasure.Types.fs"
#load "UnitsOfMeasure.Domain.fs"
#load "SliceMap.fs"

open Flips
open Flips.Types
open Flips.SliceMap
open Flips.UnitsOfMeasure
open Flips.UnitsOfMeasure.Types

type [<Measure>] Item

let decs = 
    DecisionBuilder<Item> "test" {
        for i in 1..5 ->
            Boolean
    } |> SMap.ofSeq

let e = sum (decs)

//type [<Measure>] sec

//let s1 = Domain.Scalar.Value 1.0
//let s2 = Scalar<Item>.Value s1
//let s3 = s2 + s2
//let s4 = 2.0 * s2

//let s5 = Scalar<sec>.Value (Domain.Scalar.Value 1.5)
//let x = s4 / s5
//x * 1.0<sec/Item>
//x / 2.0<1/sec>
//x * (Domain.Scalar.Value 1.5)
//x * 2.0
//x / 2.0
//let x1 = Scalar<sec>.Value (Domain.Scalar.Value 1.5)
//x1 * 1.0<1/sec> + (Domain.Scalar.Value 1.5)
//let x2 = x1 * (Domain.Scalar.Value 1.5)
