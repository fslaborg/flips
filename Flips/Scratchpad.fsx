//#I @"C:\Users\matth\.nuget\packages\"

//#r @"C:\Users\matth\.nuget\packages\google.ortools\7.5.7466\lib\netstandard2.1\Google.OrTools.dll"
//#r @"C:\Users\matth\.nuget\packages\google.ortools.runtime.win-x64\7.5.7466\runtimes\win-x64\native\google-ortools-native.dll"

#load "Domain.fs"
#load "UnitsOfMeasure.fs"
//#load "SliceMap.fs"

//open System
open Flips
////open Flips.Domain
open Flips.UnitsOfMeasure

type [<Measure>] sec
type [<Measure>] Item

let s1 = Domain.Scalar.Value 1.0
let s2 = Scalar.Value ((1.0<Item>), s1)
let s3 = s2 * 1.0<sec>
let s4 = s2 + 1.0<Item>

let d = Domain.Decision.createContinuous "test" 0.0 infinity
let d1 = Decision.Value (1.0<Item>, d)
let e1 = d1 * 1.0<sec>
let e2 = 1.0<Item * sec> + e1
let e3 = e2 + 2.5<Item * sec>
//let example1 () =
//    let products = ["Hamburger"; "Taco"; "Pizza"]
//    let locations = ["Woodstock"; "Sellwood"; "Hawthorne"]
//    let revenue = 
//        [
//            ("Hamburger", "Woodstock"), 1.40
//            ("Hamburger", "Sellwood"),  1.50
//            ("Hamburger", "Hawthorne"), 1.70
//            ("Taco",      "Woodstock"), 2.40
//            ("Taco",      "Sellwood"),  2.10
//            ("Taco",      "Hawthorne"), 2.30
//            ("Pizza",     "Woodstock"), 3.70
//            ("Pizza",     "Sellwood"),  3.50
//            ("Pizza",     "Hawthorne"), 3.30
//        ] |> Map.ofList

//    let allocateDecision =
//        [ for p in products do
//            for l in locations ->
//                let name = sprintf "Allocation|%s_%s" p l
//                (p, l), Decision.createContinuous name 0.0 infinity
//        ] |> Map.ofList

//    let totalRevenue =
//        [for p in products do
//            for l in locations ->
//                revenue.[l, p] * allocateDecision.[l, p]
//        ] |> List.sum

//    ()



//type Product = Product of string
//type Location = Location of string

//let example2 () =
//    let products = ["Hamburger"; "Taco"; "Pizza"] |> List.map Product
//    let locations = ["Woodstock"; "Sellwood"; "Hawthorne"] |> List.map Location
//    let revenue = 
//        [
//            (Product "Hamburger", Location "Woodstock"), 1.40
//            (Product "Hamburger", Location "Sellwood"),  1.50
//            (Product "Hamburger", Location "Hawthorne"), 1.70
//            (Product "Taco",      Location "Woodstock"), 2.40
//            (Product "Taco",      Location "Sellwood"),  2.10
//            (Product "Taco",      Location "Hawthorne"), 2.30
//            (Product "Pizza",     Location "Woodstock"), 3.70
//            (Product "Pizza",     Location "Sellwood"),  3.50
//            (Product "Pizza",     Location "Hawthorne"), 3.30
//        ] |> Map.ofList

//    let allocateDecision =
//        [ for p in products do
//            for l in locations ->
//                let name = sprintf "Allocation|%A_%A" p l
//                (p, l), Decision.createContinuous name 0.0 infinity
//        ] |> Map.ofList


//    // Incorrect
//    let totalRevenue =
//        [for p in products do
//            for l in locations ->
//                revenue.[l, p] * allocateDecision.[l, p]
//        ] |> List.sum

//    // Correct
//    let totalRevenue =
//        [for p in products do
//            for l in locations ->
//                revenue.[p, l] * allocateDecision.[p, l]
//        ] |> List.sum

//    ()


//let sparseExample () =
//    let products = ["Hamburger"; "Taco"; "Pizza"]
//    let locations = ["Woodstock"; "Sellwood"; "Hawthorne"; "Gresham"]
//    let revenue = 
//        [
//            ("Hamburger", "Woodstock"), 1.40
//            ("Hamburger", "Hawthorne"), 1.70
//            ("Hamburger", "Gresham"),   1.95
//            ("Taco",      "Sellwood"),  2.10
//            ("Taco",      "Hawthorne"), 2.30
//            ("Pizza",     "Woodstock"), 3.70
//            ("Pizza",     "Sellwood"),  3.50
//            ("Pizza",     "Gresham"),   3.30
//        ] |> Map.ofList

//    let allocateDecision =
//        [ for p in products do
//            for l in locations ->
//                let name = sprintf "Allocation|%A_%A" p l
//                (p, l), Decision.createContinuous name 0.0 infinity
//        ] |> Map.ofList

//    let inline tryMultiply (x, y) =
//        match x, y with
//        | Some a, Some b -> Some (a * b)
//        | _, _ -> None

//    // Sparse Revenue
//    let totalRevenue =
//        [for p in products do
//            for l in locations ->
//                (Map.tryFind (l, p) revenue), (Map.tryFind (l, p) allocateDecision)
//        ] 
//        |> List.choose tryMultiply
//        |> List.sum

//    ()

//sparseExample ()

//let indexes = [1..3]
//let locations = ["CityA"; "CityB"; "CityC"]

//// Creating a Map of decisions without the DecisionBuilder
//let withoutDecisionBuilder =
//    [for i in indexes do
//        for l in locations ->
//            let name = sprintf "Test|%i_%s" i l
//            let decisionType = DecisionType.Continuous (0.0, infinity)
//            (i, l), Decision.create name decisionType
//    ] |> Map.ofList

//// Creating a Map of decisions with the DecisionBuilder
//let withDecisionBuilder =
//    DecisionBuilder "Test" {
//        for i in indexes do
//            for l in locations ->
//                Continuous (0.0, infinity)
//    } |> Map.ofSeq



//let testDecisions = 
//    DecisionBuilder "TestDecisions" {
//        for i in x do
//            for j in y ->
//                Boolean 
//    } |> SMap2.ofSeq

//testDecisions

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

//let x = SMap.ofList [for i in 1..5 -> i, i]
//x.[1]

//let x = SMap.ofList [for i in 1..5 -> i, i]
//x.[GreaterThan 3]

//let x = SMap.ofList [for i in 1..5 -> i, i]
//x.[GreaterOrEqual 3]

//let x = SMap.ofList [for i in 1..5 -> i, i]
//x.[LessThan 3]

//let x = SMap.ofList [for i in 1..5 -> i, i]
//x.[LessOrEqual 3]

//let x = SMap.ofList [for i in 1..5 -> i, i]
//x.[Between (3, 4)]

//let x = SMap.ofList [for i in 1..5 -> i, i]
//let indexSet = Set.ofList [2; 4]
//x.[In indexSet]

//let x = SMap.ofList [for i in 1..5 -> i, i]
//let indexSet = Set.ofList [2; 4]
//x.[NotIn indexSet]

//let x = SMap.ofList [for i in 1..5 -> i, i]
//let isDivisibleBy2 x = x % 2 = 0
//x.[Where isDivisibleBy2]

//let x1 = SMap.ofList [for i in 1..5 -> i, i]
//x1.[All]



//let x2 = [for i in 1..3 do
//            for j in 1..3 ->
//                    (i, j), i + j]
//        |> SMap2.ofList
//x2.[1, 2]

//x2.[All, All]

//let x = SMap.ofList [for i in 1..3 -> i, i]
//let y = SMap.ofList [for i in 2..5 -> i, i]
//x + y

//let x = SMap.ofList [for i in 1..3 -> i, i]
//let y = SMap.ofList [for i in 2..5 -> i, i]
//x .* y

//let x = SMap2.ofList [
//    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
//    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
//    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
//]
//x.[GreaterThan 1, LessThan "b"]

//let x = SMap2.ofList [
//    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
//    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
//    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
//]
//x.[GreaterOrEqual 2, LessOrEqual "b"]


//type City = City of string
//type Index = Index of int
//let x = SMap2.ofList [
//    (Index 1, City "a"), 2.0; (Index 1, City "b"), 2.0; (Index 1, City "c"), 2.1; 
//    (Index 2, City "a"), 3.0; (Index 2, City "b"), 1.0; (Index 2, City "c"), 2.3; 
//    (Index 3, City "a"), 4.0; (Index 3, City "b"), 1.5; (Index 3, City "c"), 2.4; 
//]
//x
//x.[GreaterOrEqual (Index 2), LessOrEqual (City "b")]

//let x = array2D [ [ 1; 2]; [3; 4] ]
//let y = x.[1, 1]
//let xSlice = x.[*, 1]

//let x = SMap2.ofList [
//    (1, "a"), 2.0; (1, "b"), 2.0; (1, "c"), 2.1; 
//    (2, "a"), 3.0; (2, "b"), 1.0; (2, "c"), 2.3; 
//    (3, "a"), 4.0; (3, "b"), 1.5; (3, "c"), 2.4; 
//]
//let a = x.[1, "a"]
//let b = x.[GreaterThan 1, "a"]
//let c = x.[GreaterThan 1, LessThan "b"]

//let y = SMap2.reKey (fun (i, c) -> c, i) x
//y

//let x = Scalar 0.01358385106
//let y = Scalar 21.99286698
//let z = x + y - y
//z.ToString()
//x.ToString()
//x = y


//let (Scalar n) = z
//let n1 = System.BitConverter.DoubleToInt64Bits n
//let x1 = System.BitConverter.DoubleToInt64Bits 0.01358385106

//n1 - x1

//let x = SMap2.ofList [
//    (1, "a"), 1.0; (1, "b"), 2.0; (1, "c"), 3.0; 
//    (2, "a"), 4.0; (2, "b"), 5.0; (2, "c"), 6.0; 
//    (3, "a"), 7.0; (3, "b"), 8.0; (3, "c"), 9.0; 
//]

//let y = SMap.ofList [(1, 1.0); (2, 2.0); (3, 3.0)]

////x .* y

//let x = SMap2.ofList [
//    (1, "a"), 1.0; (1, "b"), 2.0; (1, "c"), 3.0; 
//    (2, "a"), 4.0; (2, "b"), 5.0; (2, "c"), 6.0; 
//    (3, "a"), 7.0; (3, "b"), 8.0; (3, "c"), 9.0; 
//]
//let z = SMap.ofList [("a", 1.0); ("b", 2.0); ("c", 3.0)]

//x .* z

//z .* x

//let x = 
//    DecisionBuilder "WaterSent" {
//        for i in ["a"; "b"] do
//            for j in 1..4 do
//                for k in [1M; 2M] do
//                    for l in 1..3 do
//                        for m in [100M; 250M] do
//                            for n in 1..2 do
//                                for p in 1..2 do
//                                    for q in 1..2 -> 
//                                        Continuous (0.0, infinity)
//    } |> Map.ofSeq

type DecisionName = DecisionName of string

[<CustomEquality; CustomComparison>]
type Scalar<[<Measure>] 'Measure> (f:float<'Measure>) =
    member this.Value = f

    static member (*) (s:Scalar<_>, f:float<_>) =
        Scalar (s.Value * f)

    static member (*) (f:float<_>, s:Scalar<_>) =
        Scalar (s.Value * f)

    static member (*) (lhs:Scalar<_>, rhs:Scalar<_>) =
        Scalar (lhs.Value * rhs.Value)

    static member inline Zero = Scalar (FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> 0.0)

type DecisionType<[<Measure>] 'Measure> =
    | Boolean
    | Integer of LowerBound:float<'Measure> * UpperBound:float<'Measure>
    | Continuous of LowerBound:float<'Measure> * UpperBound:float<'Measure>

type Decision<[<Measure>] 'Measure> (decisionName:DecisionName, decisionType:DecisionType<'Measure>) =
    member this.Name = decisionName
    member this.DecisionType = decisionType

    static member inline (*) (d:Decision<_>, f:float<_>) =
        (LinearExpression<_,_>.OfDecision d) * f

and LinearExpression<[<Measure>] 'CoefMeasure, [<Measure>] 'DecisionMeasure>
    (
        names : Set<DecisionName>,
        coefficients : Map<DecisionName, Scalar<'CoefMeasure>>,
        decisions : Map<DecisionName, Decision<'DecisionMeasure>>,
        offset : Scalar<'CoefMeasure * 'DecisionMeasure>
    ) =
        member this.Names = names
        member this.Coefficients = coefficients
        member this.Decisions = decisions
        member this.Offset = offset

        static member OfFloat f =
            LinearExpression (Set.empty, Map.empty, Map.empty, Scalar f)

        static member OfScalar s =
            LinearExpression (Set.empty, Map.empty, Map.empty, s)

        static member OfDecision<[<Measure>] 'DecisionMeasure> (d:Decision<'DecisionMeasure>) : LinearExpression<1, 'DecisionMeasure> =
            let names = Set.ofList [d.Name]
            let coefs = Map.ofList [d.Name, Scalar 1.0<1>]
            let decs = Map.ofList [d.Name, d]
            let offset = Scalar<_>.Zero
            LinearExpression (names, coefs, decs, offset)

        static member (*) (expr:LinearExpression<'a, 'b>, f:float<'c>) : LinearExpression<'a * 'c, 'b> =
            let newCoefs = Map.map (fun k v -> v * f) expr.Coefficients
            let newOffset = expr.Offset * f
            LinearExpression (names, newCoefs, decs, newOffset)