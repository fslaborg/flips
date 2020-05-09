#I @"C:\Users\matth\.nuget\packages\"

#r @"C:\Users\matth\.nuget\packages\google.ortools\7.5.7466\lib\netstandard2.1\Google.OrTools.dll"
//#r @"C:\Users\matth\.nuget\packages\google.ortools.runtime.win-x64\7.5.7466\runtimes\win-x64\native\google-ortools-native.dll"

#load "Domain.fs"
#load "Solve.fs"
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

let x1 = SMap.ofList [for i in 1..5 -> i, i]
x1.[1]

let x = SMap.ofList [for i in 1..5 -> i, i]
x.[GreaterThan 3]

let x = SMap.ofList [for i in 1..5 -> i, i]
x.[GreaterOrEqual 3]

let x = SMap.ofList [for i in 1..5 -> i, i]
x.[LessThan 3]

let x1 = SMap.ofList [for i in 1..5 -> i, i]
x1.[All]



let x2 = [for i in 1..3 do
            for j in 1..3 ->
                    (i, j), i + j]
        |> SMap2.ofList
x2.[1, 2]

x2.[All, All]


