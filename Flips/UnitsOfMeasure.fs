module Flips.Domain.UnitsOfMeasure

open Flips


type Scalar<[<Measure>] 'Measure> = 
    | Value of Domain.Scalar
with

    static member (+) (Value lhs:Scalar<'Measure>, Value rhs:Scalar<'Measure>) =
        Scalar<'Measure>.Value (lhs + rhs)

    static member (+) (Value lhs:Scalar<1>, rhs:Domain.Scalar) =
        lhs + rhs

    static member (+) (lhs:Domain.Scalar, Value rhs:Scalar<1>) =
        lhs + rhs

    static member (+) (Value s:Scalar<'Measure>, f:float<'Measure>) =
        Scalar<'Measure>.Value (s + float f)

    static member (+) (f:float<'Measure>, s:Scalar<'Measure>) =
        s + f

    static member (*) (Value s:Scalar<'LMeasure>, f:float<'RMeasure>) =
        Scalar<'LMeasure 'RMeasure>.Value (s * float f)

    static member (*) (f:float<_>, s:Scalar<_>) =
        s * f

    static member (*) (Value lhs:Scalar<'LMeasure>, Value rhs:Scalar<'RMeasure>) =
        Scalar<'LMeasure 'RMeasure>.Value (lhs * rhs)

    static member (*) (Value lhs:Scalar<'Measure>, Domain.Scalar.Value rhs:Domain.Scalar) =
        Scalar<'Measure>.Value (lhs * rhs)

    static member (*) (lhs:Domain.Scalar, rhs:Scalar<'Measure>) =
        rhs * lhs

    static member (-) (Value lhs:Scalar<'LMeasure>, Value rhs:Scalar<'RMeasure>) =
        Scalar<'LMeasure 'RMeasure>.Value (lhs - rhs)

    static member (-) (Value lhs:Scalar<1>, rhs:Domain.Scalar) =
        lhs - rhs

    static member (-) (lhs:Domain.Scalar, Value rhs:Scalar<1>) =
        lhs - rhs

    static member (-) (Value s:Scalar<'Measure>, f:float<'Measure>) =
        Scalar<'Measure>.Value (s - float f)

    static member (-) (f:float<'Measure>, Value s:Scalar<'Measure>) =
        Scalar<'Measure>.Value (float f - s)

    static member (/) (f:float<'LMeasure>, Value s:Scalar<'RMeasure>) =
        Scalar<'LMeasure / 'RMeasure>.Value (float f / s)

    static member (/) (Value s:Scalar<'LMeasure>, f:float<'RMeasure>) =
        Scalar<'LMeasure / 'RMeasure>.Value (s / float f)

    static member (/) (Value lhs:Scalar<'LMeasure>, Value rhs:Scalar<'RMeasure>) =
        Scalar<'LMeasure / 'RMeasure>.Value (lhs / rhs)

    static member (/) (Value lhs:Scalar<'Measure>, rhs:Domain.Scalar) =
        Scalar<'Measure>.Value (lhs / rhs)

    static member (/) (lhs:Domain.Scalar, Value rhs:Scalar<'Measure>) =
        Scalar<1/'Measure>.Value (lhs / rhs)

    static member inline Zero = Scalar<_>.Value (Scalar.Zero)


type Decision<[<Measure>] 'Measure> =
    | Value of Domain.Decision
with

    static member (+) (Value lD:Decision<'Measure>, Value rD:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (lD + rD)

    static member (+) (Value d:Decision<'Measure>, f:float<'Measure>) =
        LinearExpression<'Measure>.Value (d + float f)

    static member (+) (f:float<'Measure>, d:Decision<'Measure>) =
        d + f

    static member (+) (Value d:Decision<'Measure>, Scalar.Value s:Scalar<'Measure>) =
        LinearExpression<'Measure>.Value (d + s)

    static member (+) (s:Scalar, d:Decision) =
        d + s

    static member (*) (Value d:Decision<'LMeasure>, f:float<'RMeasure>) =
        LinearExpression<'LMeasure 'RMeasure>.Value (d * float f)

    static member (*) (f:float<'LMeasure>, d:Decision<'RMeasure>) =
        d * f

    static member (*) (Value d:Decision<'LMeasure>, Scalar.Value s:Scalar<'RMeasure>) =
        LinearExpression<'LMeasure 'RMeasure>.Value (d * s)

    static member (*) (s:Scalar<'LMeasure>, d:Decision<'RMeasure>) =
        d * s

    static member (-) (Value lD:Decision<'Measure>, Value rD:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (lD - rD)

    static member (-) (Value d:Decision<'Measure>, f:float<'Measure>) =
        LinearExpression<'Measure>.Value (d - float f)

    static member (-) (f:float<'Measure>, Value d:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (float f - d)

    static member (-) (Scalar.Value s:Scalar<'Measure>, Value d:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (s - d)

    static member (-) (Value d:Decision<'Measure>, Scalar.Value s:Scalar<'Measure>) =
        LinearExpression<'Measure>.Value (d - s)

    //static member (<==) (decision:Decision<'Measure>, f:float) =
    //    LinearExpression.OfDecision decision <== f

    //static member (<==) (decision:Decision<'Measure>, scalar:Scalar) =
    //    LinearExpression.OfDecision decision <== scalar

    //static member (<==) (decision:Decision<'Measure>, rhsDecision:Decision) =
    //    LinearExpression.OfDecision decision <== rhsDecision

    //static member (<==) (decision:Decision<'Measure>, expr:LinearExpression) =
    //    LinearExpression.OfDecision decision <== expr

    //static member (==) (decision:Decision<'Measure>, f:float) =
    //    LinearExpression.OfDecision decision == f

    //static member (==) (decision:Decision<'Measure>, scalar:Scalar) =
    //    LinearExpression.OfDecision decision == scalar

    //static member (==) (decision:Decision<'Measure>, rhsDecision:Decision) =
    //    LinearExpression.OfDecision decision == rhsDecision

    //static member (==) (decision:Decision<'Measure>, expr:LinearExpression) =
    //    LinearExpression.OfDecision decision == expr

    //static member (>==) (decision:Decision<'Measure>, f:float) =
    //    LinearExpression.OfDecision decision >== f

    //static member (>==) (decision:Decision<'Measure>, scalar:Scalar) =
    //    LinearExpression.OfDecision decision >== scalar

    //static member (>==) (decision:Decision<'Measure>, rhsDecision:Decision) =
    //    LinearExpression.OfDecision decision >== rhsDecision

    //static member (>==) (decision:Decision<'Measure>, expr:LinearExpression) =
    //    LinearExpression.OfDecision decision >== expr

and LinearExpression<[<Measure>] 'Measure> =
    | Value of Domain.LinearExpression
    with

        static member inline Zero =
            let expr = LinearExpression (Set.empty, Map.empty, Map.empty, Scalar.Zero)
            LinearExpression.Value expr

        static member (+) (Value lExpr:LinearExpression<'Measure>, Value rExpr:LinearExpression<'Measure>) =
            LinearExpression<'Measure>.Value (lExpr + rExpr)

        static member (+) (Value expr:LinearExpression<'Measure>, f:float<'Measure>) =
            LinearExpression<'Measure>.Value (expr + float f)

        static member (+) (f:float<'Measure>, expr:LinearExpression<'Measure>) =
            expr + f

        static member (+) (Value expr:LinearExpression<'Measure>, Scalar.Value s:Scalar<'Measure>) =
            LinearExpression<'Measure>.Value (expr + s)

        static member (+) (s:Scalar<'Measure>, expr:LinearExpression<'Measure>) =
            expr + s

        static member (+) (Value expr:LinearExpression<'Measure>, Decision.Value d:Decision<'Measure>) =
            LinearExpression<'Measure>.Value (expr + d)

        static member (+) (d:Decision<'Measure>, expr:LinearExpression<'Measure>) =
            expr + d

        static member (*) (Value expr:LinearExpression<'LMeasure>, f:float<'RMeasure>) =
            LinearExpression<'Measure>.Value (expr + float f)

        static member (*) (f:float<'LMeasure>, expr:LinearExpression<'RMeasure>) =
            expr * f

        static member (*) (Value expr:LinearExpression<'LMeasure>, Scalar.Value s:Scalar<'RMeasure>) =
            LinearExpression<'LMeasure 'RMeasure>.Value (expr + s)

        static member (*) (scalar:Scalar<'LMeasure>, expr:LinearExpression<'RMeasure>) =
            expr * scalar

        static member (-) (Value expr:LinearExpression<'Measure>, f:float<'Measure>) =
            LinearExpression<'Measure>.Value (expr - float f)

        static member (-) (f:float<'Measure>, Value expr:LinearExpression<'Measure>) =
            LinearExpression<'Measure>.Value (float f - expr)

        static member (-) (Value expr:LinearExpression<'Measure>, Scalar.Value s:Scalar<'Measure>) =
            LinearExpression<'Measure>.Value (expr - s)

        static member (-) (Scalar.Value s:Scalar<'Measure>, Value expr:LinearExpression<'Measure>) =
            LinearExpression<'Measure>.Value (s - expr)

        static member (-) (Value expr:LinearExpression<'Measure>, Decision.Value d:Decision<'Measure>) =
            LinearExpression<'Measure>.Value (expr - d)

        static member (-) (Decision.Value d:Decision<'Measure>, Value expr:LinearExpression<'Measure>) =
            LinearExpression<'Measure>.Value (d - expr)

        static member (-) (Value lExpr:LinearExpression<'Measure>, Value rExpr:LinearExpression<'Measure>) =
            LinearExpression<'Measure>.Value (lExpr - rExpr)

        //static member (<==) (lhs:LinearExpression, rhs:float) =
        //    Inequality (lhs, LessOrEqual, LinearExpression.OfFloat rhs)

        //static member (<==) (lhs:LinearExpression, rhs:Scalar) =
        //    Inequality (lhs, LessOrEqual, LinearExpression.OfScalar rhs)

        //static member (<==) (lhs:LinearExpression, rhs:Decision) =
        //    Inequality (lhs, LessOrEqual, LinearExpression.OfDecision rhs)

        //static member (<==) (lhs:LinearExpression, rhs:LinearExpression) =
        //    Inequality (lhs, LessOrEqual, rhs)

        //static member (==) (lhs:LinearExpression, rhs:float) =
        //    Equality (lhs, LinearExpression.OfFloat rhs)

        //static member (==) (lhs:LinearExpression, rhs:Scalar) =
        //    Equality (lhs, LinearExpression.OfScalar rhs)

        //static member (==) (lhs:LinearExpression, rhs:Decision) =
        //    Equality (lhs, LinearExpression.OfDecision rhs)

        //static member (==) (lhs:LinearExpression, rhs:LinearExpression) =
        //    Equality (lhs, rhs)

        //static member (>==) (lhs:LinearExpression, rhs:float) =
        //    Inequality (lhs, GreaterOrEqual, LinearExpression.OfFloat rhs)

        //static member (>==) (lhs:LinearExpression, rhs:Scalar) =
        //    Inequality (lhs, GreaterOrEqual, LinearExpression.OfScalar rhs)

        //static member (>==) (lhs:LinearExpression, rhs:Decision) =
        //    Inequality (lhs, GreaterOrEqual, LinearExpression.OfDecision rhs)

        //static member (>==) (lhs:LinearExpression, rhs:LinearExpression) =
        //    Inequality (lhs, GreaterOrEqual, rhs)