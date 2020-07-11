﻿namespace Flips.UnitsOfMeasure.Types

open Flips.Types

type Scalar<[<Measure>] 'Measure> = 
    | Value of Scalar
with

    static member (+) (Value lhs:Scalar<'Measure>, Value rhs:Scalar<'Measure>) =
        Scalar<'Measure>.Value (lhs + rhs)

    static member (+) (Value s:Scalar<'Measure>, f:float<'Measure>) =
        Scalar<'Measure>.Value (s + float f)

    static member (+) (f:float<'Measure>, s:Scalar<'Measure>) =
        s + f

    static member (*) (Value s:Scalar<'LMeasure>, f:float<'RMeasure>) =
        Scalar<'LMeasure 'RMeasure>.Value (s * float f)

    static member (*) (f:float<'LMeasure>, s:Scalar<'RMeasure>) =
        s * f

    static member (*) (Value lhs:Scalar<'LMeasure>, Value rhs:Scalar<'RMeasure>) =
        Scalar<'LMeasure 'RMeasure>.Value (lhs * rhs)

    static member (-) (Value lhs:Scalar<'LMeasure>, Value rhs:Scalar<'RMeasure>) =
        Scalar<'LMeasure 'RMeasure>.Value (lhs - rhs)

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

    static member inline Zero = Scalar<_>.Value (Scalar.Zero)


type Decision<[<Measure>] 'Measure> =
    | Value of Decision
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

    static member (-) (Value l:Decision<'Measure>, Value r:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (l - r)

    static member (-) (Value d:Decision<'Measure>, f:float<'Measure>) =
        LinearExpression<'Measure>.Value (d - float f)

    static member (-) (f:float<'Measure>, Value d:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (float f - d)

    static member (-) (Scalar.Value s:Scalar<'Measure>, Value d:Decision<'Measure>) =
        LinearExpression<'Measure>.Value (s - d)

    static member (-) (Value d:Decision<'Measure>, Scalar.Value s:Scalar<'Measure>) =
        LinearExpression<'Measure>.Value (d - s)

    static member (<==) (Value l:Decision<'Measure>, r:float<'Measure>) =
        l <== float r

    static member (<==) (l:float<'Measure>, Value r:Decision<'Measure>) =
        float l <== r

    static member (<==) (Value l:Decision<'Measure>, Scalar.Value r:Scalar<'Measure>) =
        l <== r

    static member (<==) (Scalar.Value l:Scalar<'Measure>, Value r:Decision<'Measure>) =
        l <== r

    static member (<==) (Value l:Decision<'Measure>, Value r:Decision<'Measure>) =
        l <== r

    static member (==) (Value l:Decision<'Measure>, r:float<'Measure>) =
        l == float r

    static member (==) (l:float<'Measure>, Value r:Decision<'Measure>) =
        float l == r

    static member (==) (Value l:Decision<'Measure>, r:Scalar) =
        l == r

    static member (==) (l:Scalar, Value r:Decision<'Measure>) =
        l == r

    static member (==) (Value l:Decision<'Measure>, Value r:Decision<'Measure>) =
        l == r

    static member (>==) (Value l:Decision<'Measure>, r:float<'Measure>) =
        l >== float r

    static member (>==) (l:float<'Measure>, Value r:Decision<'Measure>) =
        float l >== r

    static member (>==) (Value l:Decision<'Measure>, Scalar.Value r:Scalar<'Measure>) =
        l >== r

    static member (>==) (Scalar.Value l:Scalar<'Measure>, Value r:Decision<'Measure>) =
        l >== r

    static member (>==) (Value l:Decision<'Measure>, Value r:Decision<'Measure>) =
        l >== r


and LinearExpression<[<Measure>] 'Measure> =
    | Value of LinearExpression
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

    static member (<==) (Value l:LinearExpression<'Measure>, r:float<'Measure>) =
        l <== float r

    static member (<==) (l:float<'Measure>, Value r:LinearExpression<'Measure>) =
        float l <== r

    static member (<==) (Value l:LinearExpression<'Measure>, Scalar.Value r:Scalar<'Measure>) =
        l <== r

    static member (<==) (Scalar.Value l:Scalar<'Measure>, Value r:LinearExpression<'Measure>) =
        l <== r

    static member (<==) (Value l:LinearExpression<'Measure>, Decision.Value r:Decision<'Measure>) =
        l <== r

    static member (<==) (Decision.Value l:Decision<'Measure>, Value r:LinearExpression<'Measure>) =
        l <== r

    static member (<==) (Value l:LinearExpression<'Measure>, Value r:LinearExpression<'Measure>) =
        l <== r

    static member (==) (Value l:LinearExpression<'Measure>, r:float<'Measure>) =
        l == float r

    static member (==) (l:float<'Measure>, Value r:LinearExpression<'Measure>) =
        float l == r

    static member (==) (Value l:LinearExpression<'Measure>, Scalar.Value r:Scalar<'Measure>) =
        l == r

    static member (==) (Scalar.Value l:Scalar<'Measure>, Value r:LinearExpression<'Measure>) =
        l == r

    static member (==) (Value l:LinearExpression<'Measure>, Decision.Value r:Decision<'Measure>) =
        l == r

    static member (==) (Decision.Value l:Decision<'Measure>, Value r:LinearExpression<'Measure>) =
        l == r

    static member (==) (Value l:LinearExpression<'Measure>, Value r:LinearExpression<'Measure>) =
        l == r

    static member (>==) (Value l:LinearExpression<'Measure>, r:float<'Measure>) =
        l >== float r

    static member (>==) (l:float<'Measure>, Value r:LinearExpression<'Measure>) =
        float l >== r

    static member (>==) (Value l:LinearExpression<'Measure>, Scalar.Value r:Scalar<'Measure>) =
        l >== r

    static member (>==) (Scalar.Value l:Scalar<'Measure>, Value r:LinearExpression<'Measure>) =
        l >== r

    static member (>==) (Value l:LinearExpression<'Measure>, Decision.Value r:Decision<'Measure>) =
        l >== r

    static member (>==) (Decision.Value l:Decision<'Measure>, Value r:LinearExpression<'Measure>) =
        l >== r

    static member (>==) (Value l:LinearExpression<'Measure>, Value r:LinearExpression<'Measure>) =
        l >== r

