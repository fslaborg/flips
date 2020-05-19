#I @"C:\Users\matth\.nuget\packages\"

#r @"C:\Users\matth\.nuget\packages\google.ortools\7.5.7466\lib\netstandard2.1\Google.OrTools.dll"
//#r @"C:\Users\matth\.nuget\packages\google.ortools.runtime.win-x64\7.5.7466\runtimes\win-x64\native\google-ortools-native.dll"

#load "Domain.fs"
//#load "Solve.fs"
#load "SliceMap.fs"

open System
open Flips
open Flips.Domain
open Flips.SliceMap

//let profit = 
//    [
//        (("Woodstock", "Hamburger"), 1.50); (("Sellwood", "Hamburger"), 1.40); (("Portland", "Hamburger"), 1.90)
//        (("Woodstock", "HotDog"   ), 1.20); (("Sellwood", "HotDog"   ), 1.50); (("Portland", "HotDog"   ), 1.80)
//        (("Woodstock", "Pizza"    ), 2.20); (("Sellwood", "Pizza"    ), 1.70); (("Portland", "Pizza"    ), 2.00)
//    ] |> SMap2.ofList

//let x = profit * 2.0
//x
//let hamburgerProfit = 1.50
//let hotdogProfit = 1.20
//let hamburgerBuns = 300.0
//let hotdogBuns = 200.0
//let hamburgerWeight = 0.5
//let hotdogWeight = 0.4
//let maxWeight = 800.0

//let numberOfHamburgers = Decision.createContinuous "NumberOfHamburgers" 0.0 infinity
//let numberOfHotdogs = Decision.createContinuous "NumberOfHotDogs" 0.0 infinity

//let objectiveExpression = hamburgerProfit * numberOfHamburgers + hotdogProfit * numberOfHotdogs
//let objective = Objective.create "MaximizeRevenue" Maximize objectiveExpression

//let maxHamburgerConstraint = Constraint.create "MaxHamburger" (numberOfHamburgers <== hamburgerBuns)
//let maxHotDogConstraint = Constraint.create "MaxHotDog" (numberOfHotdogs <== hotdogBuns)
//let maxWeightConstraint = Constraint.create "MaxWeight" (numberOfHotdogs * hotdogWeight + numberOfHamburgers * hamburgerWeight <== maxWeight)

//let model =
//    Model.create objective
//    |> Model.addConstraint maxHamburgerConstraint
//    |> Model.addConstraint maxHotDogConstraint
//    |> Model.addConstraint maxWeightConstraint

//let settings = {
//    SolverType = SolverType.CBC
//    MaxDuration = 10_000L
//    WriteLPFile = None
//}

//let result = Solve.solve settings model
//printfn "%A" result

//let f1 = SMap2.ofList [
//    (1, "a"), 1.0; (1, "b"), 2.0; (1, "c"), 3.0; 
//    (2, "a"), 1.0; (2, "b"), 2.0; (2, "c"), 3.0; 
//    (3, "a"), 1.0; (3, "b"), 2.5; (3, "c"), 3.0; 
//]

//let f2 = SMap2.ofList [
//    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
//    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
//    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
//]

//f1 + f2
//f1 .* f2

let x = SMap.ofList [for i in 1..5 -> i, i]
x.[1]

let x = SMap.ofList [for i in 1..5 -> i, i]
x.[GreaterThan 3]

let x = SMap.ofList [for i in 1..5 -> i, i]
x.[GreaterOrEqual 3]

let x = SMap.ofList [for i in 1..5 -> i, i]
x.[LessThan 3]

let x = SMap.ofList [for i in 1..5 -> i, i]
x.[LessOrEqual 3]

let x = SMap.ofList [for i in 1..5 -> i, i]
x.[Between (3, 4)]

let x = SMap.ofList [for i in 1..5 -> i, i]
let indexSet = Set.ofList [2; 4]
x.[In indexSet]

let x = SMap.ofList [for i in 1..5 -> i, i]
let indexSet = Set.ofList [2; 4]
x.[NotIn indexSet]

let x = SMap.ofList [for i in 1..5 -> i, i]
let isDivisibleBy2 x = x % 2 = 0
x.[Where isDivisibleBy2]

let x1 = SMap.ofList [for i in 1..5 -> i, i]
x1.[All]



let x2 = [for i in 1..3 do
            for j in 1..3 ->
                    (i, j), i + j]
        |> SMap2.ofList
x2.[1, 2]

x2.[All, All]

let x = SMap.ofList [for i in 1..3 -> i, i]
let y = SMap.ofList [for i in 2..5 -> i, i]
x + y

let x = SMap.ofList [for i in 1..3 -> i, i]
let y = SMap.ofList [for i in 2..5 -> i, i]
x .* y

let x = SMap2.ofList [
    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
]
x.[GreaterThan 1, LessThan "b"]

let x = SMap2.ofList [
    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
]
x.[GreaterOrEqual 2, LessOrEqual "b"]


type City = City of string
type Index = Index of int
let x = SMap2.ofList [
    (Index 1, City "a"), 2.0; (Index 1, City "b"), 2.0; (Index 1, City "c"), 2.1; 
    (Index 2, City "a"), 3.0; (Index 2, City "b"), 1.0; (Index 2, City "c"), 2.3; 
    (Index 3, City "a"), 4.0; (Index 3, City "b"), 1.5; (Index 3, City "c"), 2.4; 
]
x
x.[GreaterOrEqual (Index 2), LessOrEqual (City "b")]

let x = array2D [ [ 1; 2]; [3; 4] ]
let y = x.[1, 1]
let xSlice = x.[*, 1]

let x = SMap2.ofList [
    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
]
let a = x.[1, "a"]
let b = x.[GreaterThan 1, "a"]
let c = x.[GreaterThan 1, LessThan "b"]

let y = SMap2.reKey (fun (i, c) -> c, i) x
y

let x = Scalar 0.01358385106
let y = Scalar 21.99286698
let z = x + y - y
z.ToString()
x.ToString()
x = y


let (Scalar n) = z
let n1 = System.BitConverter.DoubleToInt64Bits n
let x1 = System.BitConverter.DoubleToInt64Bits 0.01358385106

n1 - x1

let x = SMap2.ofList [
    (1, "a"), 1.0; (1, "b"), 2.0; (1, "c"), 3.0; 
    (2, "a"), 4.0; (2, "b"), 5.0; (2, "c"), 6.0; 
    (3, "a"), 7.0; (3, "b"), 8.0; (3, "c"), 9.0; 
]

let y = SMap.ofList [(1, 1.0); (2, 2.0); (3, 3.0)]

x .* y

let x = SMap2.ofList [
    (1, "a"), 1.0; (1, "b"), 2.0; (1, "c"), 3.0; 
    (2, "a"), 4.0; (2, "b"), 5.0; (2, "c"), 6.0; 
    (3, "a"), 7.0; (3, "b"), 8.0; (3, "c"), 9.0; 
]
let z = SMap.ofList [("a", 1.0); ("b", 2.0); ("c", 3.0)]

x .* z

z .* x