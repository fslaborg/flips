namespace Flips.UnitsOfMeasure

[<AutoOpen>]
module Types =

    open Flips
    
    /// A Decision with a UnitOfMeasure
    type Decision<[<Measure>] 'Measure> =
        | Value of Types.Decision
    with
    
        member this.Name =
          let (Value d) = this
          d.Name
    
        static member (+) (Value lD: Decision<'Measure>, Value rD: Decision<'Measure>) =
          LinearExpression<'Measure>.Value (lD + rD)
    
        static member (+) (Value d: Decision<'Measure>, f: float<'Measure>) =
          LinearExpression<'Measure>.Value (d + float f)
    
        static member (+) (f: float<'Measure>, d: Decision<'Measure>) =
          d + f
    
        static member (*) (Value d: Decision<'LMeasure>, f: float<'RMeasure>) =
          LinearExpression<'LMeasure 'RMeasure>.Value (d * float f)
    
        static member (*) (f: float<'LMeasure>, d: Decision<'RMeasure>) =
          d * f
    
        static member (-) (Value l: Decision<'Measure>, Value r: Decision<'Measure>) =
          LinearExpression<'Measure>.Value (l - r)
    
        static member (-) (Value d: Decision<'Measure>, f: float<'Measure>) =
          LinearExpression<'Measure>.Value (d - float f)
    
        static member (-) (f: float<'Measure>, Value d: Decision<'Measure>) =
          LinearExpression<'Measure>.Value (float f - d)
    
        static member (<==) (Value l: Decision<'Measure>, r: float<'Measure>) =
          l <== float r
    
        static member (<==) (l: float<'Measure>, Value r: Decision<'Measure>) =
          float l <== r
    
        static member (<==) (Value l: Decision<'Measure>, Value r: Decision<'Measure>) =
          l <== r
    
        static member (==) (Value l: Decision<'Measure>, r: float<'Measure>) =
          l == float r
    
        static member (==) (l: float<'Measure>, Value r: Decision<'Measure>) =
          float l == r
    
        static member (==) (Value l: Decision<'Measure>, Value r: Decision<'Measure>) =
          l == r
    
        static member (>==) (Value l: Decision<'Measure>, r: float<'Measure>) =
          l >== float r
    
        static member (>==) (l: float<'Measure>, Value r: Decision<'Measure>) =
          float l >== r
    
        static member (>==) (Value l: Decision<'Measure>, Value r: Decision<'Measure>) =
          l >== r
    
    
    and 
        /// A LinearExpression with a UnitOfMeausre
        [<NoComparison>]
        LinearExpression<[<Measure>] 'Measure> =
        | Value of Types.LinearExpression
        with
    
            static member inline Zero =
                LinearExpression<'Measure>.Value Flips.Types.LinearExpression.Zero
    
            static member (+) (Value lExpr: LinearExpression<'Measure>, Value rExpr: LinearExpression<'Measure>) =
                LinearExpression<'Measure>.Value (lExpr + rExpr)
    
            static member (+) (Value expr: LinearExpression<'Measure>, f: float<'Measure>) =
                LinearExpression<'Measure>.Value (expr + float f)
    
            static member (+) (f: float<'Measure>, expr: LinearExpression<'Measure>) =
                expr + f
    
            static member (+) (Value expr: LinearExpression<'Measure>, Decision.Value d: Decision<'Measure>) =
                LinearExpression<'Measure>.Value (expr + d)
    
            static member (+) (d: Decision<'Measure>, expr: LinearExpression<'Measure>) =
                expr + d
    
            static member (*) (Value expr: LinearExpression<'LMeasure>, f: float<'RMeasure>) =
                LinearExpression<'Measure>.Value (expr * float f)
    
            static member (*) (f: float<'LMeasure>, expr: LinearExpression<'RMeasure>) =
                expr * f
    
            static member (-) (Value expr: LinearExpression<'Measure>, f: float<'Measure>) =
                LinearExpression<'Measure>.Value (expr - float f)
    
            static member (-) (f: float<'Measure>, Value expr: LinearExpression<'Measure>) =
                LinearExpression<'Measure>.Value (float f - expr)
    
            static member (-) (Value expr: LinearExpression<'Measure>, Decision.Value d: Decision<'Measure>) =
                LinearExpression<'Measure>.Value (expr - d)
    
            static member (-) (Decision.Value d: Decision<'Measure>, Value expr: LinearExpression<'Measure>) =
                LinearExpression<'Measure>.Value (d - expr)
    
            static member (-) (Value lExpr: LinearExpression<'Measure>, Value rExpr: LinearExpression<'Measure>) =
                LinearExpression<'Measure>.Value (lExpr - rExpr)
    
            static member (<==) (Value l: LinearExpression<'Measure>, r: float<'Measure>) =
                l <== float r
    
            static member (<==) (l: float<'Measure>, Value r: LinearExpression<'Measure>) =
                float l <== r
    
            static member (<==) (Value l: LinearExpression<'Measure>, Decision.Value r: Decision<'Measure>) =
                l <== r
    
            static member (<==) (Decision.Value l: Decision<'Measure>, Value r: LinearExpression<'Measure>) =
                l <== r
    
            static member (<==) (Value l: LinearExpression<'Measure>, Value r: LinearExpression<'Measure>) =
                l <== r
    
            static member (==) (Value l: LinearExpression<'Measure>, r: float<'Measure>) =
                l == float r
    
            static member (==) (l: float<'Measure>, Value r: LinearExpression<'Measure>) =
                float l == r
    
            static member (==) (Value l: LinearExpression<'Measure>, Decision.Value r: Decision<'Measure>) =
                l == r
    
            static member (==) (Decision.Value l: Decision<'Measure>, Value r: LinearExpression<'Measure>) =
                l == r
    
            static member (==) (Value l: LinearExpression<'Measure>, Value r: LinearExpression<'Measure>) =
                l == r
    
            static member (>==) (Value l: LinearExpression<'Measure>, r: float<'Measure>) =
                l >== float r
    
            static member (>==) (l: float<'Measure>, Value r: LinearExpression<'Measure>) =
                float l >== r
    
            static member (>==) (Value l: LinearExpression<'Measure>, Decision.Value r: Decision<'Measure>) =
                l >== r
    
            static member (>==) (Decision.Value l: Decision<'Measure>, Value r: LinearExpression<'Measure>) =
                l >== r
    
            static member (>==) (Value l: LinearExpression<'Measure>, Value r: LinearExpression<'Measure>) =
                l >== r
    
    
    /// A DecisionType with a UnitOfMeasure
    type DecisionType<[<Measure>] 'Measure> =
        | Boolean
        | Integer of LowerBound: float<'Measure> * UpperBound: float<'Measure>
        | Continuous of LowerBound: float<'Measure> * UpperBound: float<'Measure>

    [<NoComparison>]
    type Objective<[<Measure>] 'Measure> =
        | Value of Types.Objective
    
    