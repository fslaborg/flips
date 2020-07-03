module Flips.UnitsOfMeasure

let inline private retype (x: 'T) : 'U = (# "" x: 'U #) 
let inline private stripUoM (x: '``T<'M>``) =
    let _ = x * (LanguagePrimitives.GenericOne : 'T)
    retype x :'T

type Scalar<[<Measure>] 'Measure> = 
    | Scalar of UoM:float<'Measure> * Scalar:Domain.Scalar
with

    static member (+) (Scalar (lhsUoM, lhsS):Scalar<'Measure>, Scalar (rhsUoM, rhsS):Scalar<'Measure>) =
        Scalar (lhsUoM, lhsS * rhsS)

    static member (+) (Scalar (uom, s):Scalar<'Measure>, f:float<'Measure>) =
        let unitlessF = stripUoM f
        Scalar (uom, s + unitlessF)

    static member (+) (f:float<'Measure>, s:Scalar<'Measure>) =
        s + f

    static member (*) (Scalar (uom, s):Scalar<_>, f) =
        let newUoM = uom * f
        let unitlessF = stripUoM f
        Scalar (newUoM, s * unitlessF)

    static member (*) (f:float<_>, s:Scalar<_>) =
        s * f

    static member (*) (Scalar (lhsUoM, lhsS):Scalar<_>, Scalar (rhsUoM, rhsS):Scalar<_>) =
        let newUoM = lhsUoM * rhsUoM
        Scalar (newUoM, lhsS * rhsS)

    //static member (-) (Scalar lhs:Scalar<'Measure>, Scalar rhs:Scalar<'Measure>) =
    //    Scalar (lhs - rhs)

    //static member (-) (Scalar s:Scalar<'Measure>, f:float<'Measure>) =
    //    Scalar (s - f)

    //static member (-) (f:float<'Measure>, Scalar s:Scalar<'Measure>) =
    //    Scalar (f - s)

    //static member (/) (f:float<_>, Scalar s:Scalar<_>) =
    //    Scalar (f / s)

    //static member (/) (Scalar s:Scalar<_>, f:float<_>) =
    //    Scalar (s / f)

    //static member (/) (Scalar lhs:Scalar<_>, Scalar rhs:Scalar<_>) =
    //    Scalar (lhs / rhs)

    //static member inline Zero = Scalar (FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> 0.0)

    //override this.GetHashCode () =
    //    hash this.Value

    //override this.Equals(obj) =
    //    match obj with
    //    | :? Scalar<'Measure> as s ->
    //        this.Value = s.Value
    //    | _ -> false

    //interface System.IComparable with
    //    member this.CompareTo yObj =
    //        match yObj with
    //        | :? Scalar<'Measure> as s -> compare this s
    //        | _ -> invalidArg "yObj" "Cannot compare values of different types"

type DecisionType<[<Measure>] 'Measure> =
    | DecisionType of UoM:float<'Measure> * DecisionType:Domain.DecisionType

//type Decision<[<Measure>] 'Measure> (decisionName:Domain.DecisionName, decisionType:DecisionType<'Measure>) =
//    member this.Name = decisionName
//    member this.DecisionType = decisionType
//with

//    static member inline (*) (d:Decision<_>, f:float<_>) =
//        (LinearExpression<_,_>.OfDecision d) * f

//    //static member (*) (f:float<'CoefMeasure>, decision:Decision<'DecisionMeasure>) =
//    //    LinearExpression.OfDecision decision * f

//    //static member (*) (decision:Decision<_>, scalar:Scalar<_>) =
//    //    LinearExpression.OfDecision decision * scalar

//    //static member (*) (scalar:Scalar, decision:Decision) =
//    //    LinearExpression.OfDecision decision * scalar

//    //static member (+) (decision:Decision, f:float) =
//    //    LinearExpression.OfDecision decision + f

//    //static member (+) (f:float, decision:Decision) =
//    //    LinearExpression.OfDecision decision + f

//    //static member (+) (scalar:Scalar, decision:Decision) =
//    //    LinearExpression.OfScalar scalar + decision

//    //static member (+) (decision:Decision, scalar:Scalar) =
//    //    LinearExpression.OfDecision decision + scalar

//    //static member (+) (decision:Decision, rightDecision:Decision) =
//    //    LinearExpression.OfDecision decision + rightDecision

//    //static member (+) (decision:Decision, expr:LinearExpression) =
//    //    LinearExpression.OfDecision decision + expr

//    //static member (-) (decision:Decision, f:float) =
//    //    LinearExpression.OfDecision decision - f

//    //static member (-) (f:float, decision:Decision) =
//    //    LinearExpression.OfDecision decision - f

//    //static member (-) (scalar:Scalar, decision:Decision) =
//    //    LinearExpression.OfScalar scalar + (-1.0 * decision)

//    //static member (-) (decision:Decision, scalar:Scalar) =
//    //    LinearExpression.OfDecision decision + (-1.0 * scalar)

//    //static member (-) (decision:Decision, rightDecision:Decision) =
//    //    LinearExpression.OfDecision decision + (-1.0 * rightDecision)

//    //static member (-) (decision:Decision, expr:LinearExpression) =
//    //    LinearExpression.OfDecision decision + (-1.0 * expr)

//    //static member (<==) (decision:Decision, f:float) =
//    //    LinearExpression.OfDecision decision <== f

//    //static member (<==) (decision:Decision, scalar:Scalar) =
//    //    LinearExpression.OfDecision decision <== scalar

//    //static member (<==) (decision:Decision, rhsDecision:Decision) =
//    //    LinearExpression.OfDecision decision <== rhsDecision

//    //static member (<==) (decision:Decision, expr:LinearExpression) =
//    //    LinearExpression.OfDecision decision <== expr

//    //static member (==) (decision:Decision, f:float) =
//    //    LinearExpression.OfDecision decision == f

//    //static member (==) (decision:Decision, scalar:Scalar) =
//    //    LinearExpression.OfDecision decision == scalar

//    //static member (==) (decision:Decision, rhsDecision:Decision) =
//    //    LinearExpression.OfDecision decision == rhsDecision

//    //static member (==) (decision:Decision, expr:LinearExpression) =
//    //    LinearExpression.OfDecision decision == expr

//    //static member (>==) (decision:Decision, f:float) =
//    //    LinearExpression.OfDecision decision >== f

//    //static member (>==) (decision:Decision, scalar:Scalar) =
//    //    LinearExpression.OfDecision decision >== scalar

//    //static member (>==) (decision:Decision, rhsDecision:Decision) =
//    //    LinearExpression.OfDecision decision >== rhsDecision

//    //static member (>==) (decision:Decision, expr:LinearExpression) =
//    //    LinearExpression.OfDecision decision >== expr

//and LinearExpression<[<Measure>] 'CoefMeasure, [<Measure>] 'DecisionMeasure>
//    (
//        names : Set<Domain.DecisionName>,
//        coefficients : Map<Domain.DecisionName, Scalar<'CoefMeasure>>,
//        decisions : Map<Domain.DecisionName, Decision<'DecisionMeasure>>,
//        offset : Scalar<'CoefMeasure * 'DecisionMeasure>
//    ) =
//        member this.Names = names
//        member this.Coefficients = coefficients
//        member this.Decisions = decisions
//        member this.Offset = offset

//        //static member private Equivalent (lExpr:LinearExpression<'C,'D>) (rExpr:LinearExpression<'C,'D>) =
//        //    let isEqualOffset = (lExpr.Offset = rExpr.Offset)
//        //    let leftOnlyNames = lExpr.Names - rExpr.Names
//        //    let rightOnlyNames = rExpr.Names - lExpr.Names
//        //    let overlapNames = Set.intersect lExpr.Names rExpr.Names

//        //    let leftOnlyNamesAreZero = 
//        //        leftOnlyNames
//        //        |> Set.forall (fun n -> lExpr.Coefficients.[n] = Scalar.Zero)

//        //    let rightOnlyNamesAreZero =
//        //        rightOnlyNames
//        //        |> Set.forall (fun n -> rExpr.Coefficients.[n] = Scalar.Zero)

//        //    let overlapNamesMatch =
//        //        overlapNames
//        //        |> Set.forall (fun n -> lExpr.Coefficients.[n] = rExpr.Coefficients.[n])

//        //    isEqualOffset && leftOnlyNamesAreZero && rightOnlyNamesAreZero && overlapNamesMatch

//        //override this.GetHashCode () =
//        //    hash this

//        //override this.Equals(obj) =
//        //    match obj with
//        //    | :? LinearExpression<'C,'D> as expr -> LinearExpression.Equivalent<'C,'D> this expr
//        //    | _ -> false

//        static member OfFloat f =
//            LinearExpression (Set.empty, Map.empty, Map.empty, Scalar f)

//        static member OfScalar s =
//            LinearExpression (Set.empty, Map.empty, Map.empty, s)

//        static member OfDecision<[<Measure>] 'DecisionMeasure> (d:Decision<'DecisionMeasure>) : LinearExpression<1, 'DecisionMeasure> =
//            let names = Set.ofList [d.Name]
//            let coefs = Map.ofList [d.Name, Scalar 1.0<1>]
//            let decs = Map.ofList [d.Name, d]
//            let offset = Scalar.Zero
//            LinearExpression (names, coefs, decs, offset)

//        //static member GetDecisions (expr:LinearExpression<_,_>) =
//        //    expr.Decisions
//        //    |> Map.toList
//        //    |> List.map snd
//        //    |> Set.ofList

//        //static member inline Zero =
//        //    LinearExpression (Set.empty, Map.empty, Map.empty, Scalar.Zero)

//        //static member (+) (expr:LinearExpression<'C,'D>, f:float<'C * 'D>) =
//        //    LinearExpression<'C,'D> (expr.Names, expr.Coefficients, expr.Decisions, expr.Offset + (Scalar f))

//        //static member (+) (f:float, LinearExpression (names, coefs, decs, offset):LinearExpression) =
//        //    LinearExpression (names, coefs, decs, offset + (Scalar f))

//        //static member (+) (LinearExpression (names, coefs, decs, offset):LinearExpression, scalar:Scalar) =
//        //    LinearExpression (names, coefs, decs, offset + scalar)

//        //static member (+) (scalar:Scalar, LinearExpression (names, coefs, decs, offset):LinearExpression) =
//        //    LinearExpression (names, coefs, decs, offset + scalar)

//        //static member (+) (LinearExpression (names, coefs, decs, offset):LinearExpression, decision:Decision) =
//        //    if Set.contains decision.Name names then
//        //        if decs.[decision.Name].Type <> decision.Type then
//        //            let (DecisionName name) = decision.Name
//        //            invalidArg name "Mistmatched DecisionType"

//        //        let newCoefs = Map.add decision.Name (coefs.[decision.Name] + (Scalar 1.0)) coefs
//        //        LinearExpression (names, newCoefs, decs, offset)
//        //    else
//        //        let newNames = Set.add decision.Name names
//        //        let newCoefs = Map.add decision.Name (Scalar 1.0) coefs
//        //        let newDecs = Map.add decision.Name decision decs
//        //        LinearExpression (newNames, newCoefs, newDecs, offset)

//        //static member private Merge (LinearExpression (lNames, lCoefs, lDecs, lOffset):LinearExpression, LinearExpression (rNames, rCoefs, rDecs, rOffset):LinearExpression) =
//        //    // Assume the Left LinearExpression is larget than the right
//        //    let nameOverlap = Set.intersect lNames rNames
    
//        //    for n in nameOverlap do
//        //        if lDecs.[n].Type <> rDecs.[n].Type then
//        //            let (DecisionName name) = n
//        //            invalidArg name "Cannot have mismatched DecisionTypes for same DecisionName"

//        //    let newNames = lNames + rNames

//        //    let newDecs = (lDecs, (rNames - lNames)) ||> Set.fold (fun m k -> Map.add k rDecs.[k] m)

//        //    let newCoefs =
//        //        (lCoefs, nameOverlap)
//        //        ||> Set.fold (fun m k -> Map.add k (lCoefs.[k] + rCoefs.[k]) m)
//        //        |> fun updatedCoefs -> Set.fold (fun m n -> Map.add n rCoefs.[n] m) updatedCoefs (rNames - lNames)

//        //    LinearExpression (newNames, newCoefs, newDecs, lOffset + rOffset)

//        //static member (+) (lExpr:LinearExpression, rExpr:LinearExpression) =
//        //    let (LinearExpression (lNames, _, _, _)) = lExpr
//        //    let (LinearExpression (rNames, _, _, _)) = rExpr
//        //    let lSize = Set.count lNames
//        //    let rSize = Set.count rNames

//        //    if lSize > rSize then
//        //        LinearExpression.Merge (lExpr, rExpr)
//        //    else
//        //        LinearExpression.Merge (rExpr, lExpr)

//        static member (*) (expr:LinearExpression<'a, 'b>, f:float<'c>) : LinearExpression<'a * 'c, 'b> =
//            let newCoefs = Map.map (fun k v -> v * f) expr.Coefficients
//            let newOffset = expr.Offset * f
//            LinearExpression (names, newCoefs, decs, newOffset)

//        //static member (*) (f:float, expr:LinearExpression) =
//        //    expr * f

//        //static member inline (*) (expr:LinearExpression<'A,'B>, scalar:Scalar<'C>) =
//        //    let newCoefs = Map.map (fun _ (v:Scalar<'A>) -> v * scalar) expr.Coefficients
//        //    LinearExpression (expr.Names, newCoefs, expr.Decisions, expr.Offset * scalar)

//        //static member (*) (scalar:Scalar, expr:LinearExpression) =
//        //    expr * scalar

//        //static member (-) (expr:LinearExpression, f:float) =
//        //    expr + (-1.0 * f)

//        //static member (-) (f:float, expr:LinearExpression) =
//        //    f + (-1.0 * expr)

//        //static member (-) (expr:LinearExpression, s:Scalar) =
//        //    expr + (-1.0 * s)

//        //static member (-) (s:Scalar, expr:LinearExpression) =
//        //    s + (-1.0 * expr)

//        //static member (-) (expr:LinearExpression, d:Decision) =
//        //    expr + (-1.0 * d)

//        //static member (-) (d:Decision, expr:LinearExpression) =
//        //    d + (-1.0 * expr)

//        //static member (-) (lExpr:LinearExpression, rExpr:LinearExpression) =
//        //    lExpr + (-1.0 * rExpr)

//        //static member (<==) (lhs:LinearExpression, rhs:float) =
//        //    Inequality (lhs, LessOrEqual, LinearExpression.OfFloat rhs)

//        //static member (<==) (lhs:LinearExpression, rhs:Scalar) =
//        //    Inequality (lhs, LessOrEqual, LinearExpression.OfScalar rhs)

//        //static member (<==) (lhs:LinearExpression, rhs:Decision) =
//        //    Inequality (lhs, LessOrEqual, LinearExpression.OfDecision rhs)

//        //static member (<==) (lhs:LinearExpression, rhs:LinearExpression) =
//        //    Inequality (lhs, LessOrEqual, rhs)

//        //static member (==) (lhs:LinearExpression, rhs:float) =
//        //    Equality (lhs, LinearExpression.OfFloat rhs)

//        //static member (==) (lhs:LinearExpression, rhs:Scalar) =
//        //    Equality (lhs, LinearExpression.OfScalar rhs)

//        //static member (==) (lhs:LinearExpression, rhs:Decision) =
//        //    Equality (lhs, LinearExpression.OfDecision rhs)

//        //static member (==) (lhs:LinearExpression, rhs:LinearExpression) =
//        //    Equality (lhs, rhs)

//        //static member (>==) (lhs:LinearExpression, rhs:float) =
//        //    Inequality (lhs, GreaterOrEqual, LinearExpression.OfFloat rhs)

//        //static member (>==) (lhs:LinearExpression, rhs:Scalar) =
//        //    Inequality (lhs, GreaterOrEqual, LinearExpression.OfScalar rhs)

//        //static member (>==) (lhs:LinearExpression, rhs:Decision) =
//        //    Inequality (lhs, GreaterOrEqual, LinearExpression.OfDecision rhs)

//        //static member (>==) (lhs:LinearExpression, rhs:LinearExpression) =
//        //    Inequality (lhs, GreaterOrEqual, rhs)


////and Inequality =
////| LessOrEqual
////| GreaterOrEqual

////and ConstraintExpression = 
////| Inequality of LHS:LinearExpression * Inequality * RHS:LinearExpression
////| Equality of LHS:LinearExpression * RHS:LinearExpression