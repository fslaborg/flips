namespace Flips.Types.TypeExtensions.Cplex
open ILOG.Concert
open Flips.Types
[<AutoOpen>]
module Extensions =
  type Flips.Types.DecisionType with
      member x.ToCplex =
          match x with
          | DecisionType.Boolean      -> NumVarType.Bool
          | DecisionType.Integer _    -> NumVarType.Int
          | DecisionType.Continuous _ -> NumVarType.Float

  type Flips.Types.ObjectiveSense with
      member x.ToCplex =
          match x with
          | ObjectiveSense.Maximize -> ILOG.Concert.ObjectiveSense.Maximize
          | ObjectiveSense.Minimize -> ILOG.Concert.ObjectiveSense.Minimize


namespace Flips.Types.TypeExtensions

open Flips.Types
open System.Collections.Generic
[<AutoOpen>]
module rec Extensions =
  type Flips.Types.Decision with 
      member x.LB = 
        match x with 
        | {Type = DecisionType.Continuous(v,_) } 
        | {Type = DecisionType.Integer(v,_) } -> v
        | {Type = DecisionType.Boolean } -> 0.
      member x.UB = 
        match x with 
        | {Type = DecisionType.Continuous(_,v) } 
        | {Type = DecisionType.Integer(_,v) } -> v
        | {Type = DecisionType.Boolean } -> 1.
      member x.Bounds = x.LB,x.UB

  type Flips.Types.Objective with
      member x.Name = match x with | { Name = name } -> name

  type Flips.Types.ObjectiveName with
      member x.AsString = match x with ObjectiveName n -> n

  type Flips.Types.ConstraintName with
      member x.AsString = match x with ConstraintName n -> n

  type Flips.Types.DecisionName with
      member x.AsString = match x with DecisionName n -> n
  let uom<[<Measure>]'measure> = LanguagePrimitives.FloatWithMeasure<'measure>

  type Flips.UnitsOfMeasure.Types.Objective<[<Measure>]'m> with
      member x.Objective =
        match (x: Flips.UnitsOfMeasure.Types.Objective<'m>) with 
        | Flips.UnitsOfMeasure.Types.Objective.Value o -> o
    
  type Flips.UnitsOfMeasure.Types.Decision<[<Measure>]'m> with
    
      member x.Decision =
        match (x: Flips.UnitsOfMeasure.Types.Decision<'m>) with 
        | Flips.UnitsOfMeasure.Types.Value x -> x

      member x.LB = x.Decision.LB * (uom<'m> 1.)
      member x.UB = x.Decision.UB * (uom<'m> 1.)

  type Flips.Types.Objective with
      static member getName       (x: Flips.Types.Objective) = match x with | { Name = n } -> n
      static member getExpression (x: Flips.Types.Objective) = match x with | { Expression = e } -> e

  type Flips.Types.Constraint with
      static member getName       (x: Flips.Types.Constraint) = match x with | { Name = n } -> n
      static member getExpression (x: Flips.Types.Constraint) = match x with | { Expression = e } -> e
  type Flips.Types.LinearExpression with
      static member evaluate valMap expr = Flips.Types.LinearExpression.Evaluate valMap expr
  type Flips.Types.Model with
      member x.GetDecisions() =
          seq {
              for c in x.Constraints do
                yield! c.Expression.GetDecisions()
              for o in x.Objectives do
                yield! o.Expression.GetDecisions()
          }
  /// todo for matthewcrews: review if correct and can go to Flips.Solver
  type Flips.Types.LinearExpression with
      member internal x.GetDecisions() =
          seq {
              let decisions = LinearExpression.Reduce x
              for d in decisions.Coefficients.Keys do
                yield {Name= d; Type = decisions.DecisionTypes.[d]}
          }

  /// todo for matthewcrews: review if correct and can go to Flips.Solver
  type Flips.Types.ConstraintExpression with
      member x.LHS =
          match x with
          | ConstraintExpression.Inequality(LHS=lhs)
          | ConstraintExpression.Equality(LHS=lhs) -> lhs
      member x.RHS =
          match x with
          | ConstraintExpression.Inequality(RHS=rhs)
          | ConstraintExpression.Equality(RHS=rhs) -> rhs

      member x.GetDecisions () =
          seq {
              match x with
              | ConstraintExpression.Equality(lhs,rhs) 
              | ConstraintExpression.Inequality(lhs,_,rhs) ->
                  yield! lhs.GetDecisions()
                  yield! rhs.GetDecisions()            
          }

  type Flips.Types.Objective with
      static member evaluate (solution: Flips.Solver.ISolution) (objective: Flips.Types.Objective) =
          objective.Expression
          |> Flips.Types.LinearExpression.Evaluate solution.Values

  type Flips.UnitsOfMeasure.Types.Objective<[<Measure>]'Measure> with
      static member evaluate (solution: Flips.Solver.ISolution) (Flips.UnitsOfMeasure.Types.Objective.Value objective: Flips.UnitsOfMeasure.Types.Objective<'Measure>) =
          objective.Expression
          |> Flips.Types.LinearExpression.Evaluate solution.Values
          |> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure>

  type Printer =
    static member print (expr: Flips.Types.Objective) =
      let t =
        match expr.Sense with
        | ObjectiveSense.Maximize -> "max"
        | ObjectiveSense.Minimize -> "min"
      $"{t} {expr.Name.AsString} such as: {Printer.print expr.Expression}"

    static member printWithLiterals (values:IReadOnlyDictionary<_,_>) (expr: Flips.Types.Constraint) =
      let d = 
        [for d in expr.Expression.GetDecisions() do 
          d.Name, d] |> dict
      let printed = Printer.print expr
      let r =
        d.Values 
        |> Seq.fold (fun (state:string) d -> 
          let value = string values.[d]
          state.Replace(d.Name.AsString, value)
        ) printed
      r
    static member print (expr: Flips.Types.Constraint) =
      $"{expr.Name.AsString} : {Printer.print expr.Expression}"

    static member print (expr: Flips.Types.ConstraintExpression) =
      let op = 
        match expr with
        | Inequality(inequality=GreaterOrEqual) -> ">="
        | Inequality(inequality=LessOrEqual) -> "<="
        | Equality _ -> "="
      match expr with
      | Inequality(lhs,_ ,rhs)
      | Equality (lhs, rhs) -> $"{Printer.print lhs} {op} {Printer.print rhs}"

    static member print (expr: Flips.Types.LinearExpression) =
        let rec doIt expr =
          match expr with
          | LinearExpression.Empty -> ""
          | LinearExpression.AddDecision((1.,d),Empty) -> $"{d.Name.AsString}"
          | LinearExpression.AddDecision((coef,d),Empty) -> $"(%f{coef} * {d.Name.AsString})"
          | LinearExpression.AddDecision((1.,d),expr) -> $"{d.Name.AsString} + {doIt expr}"
          | LinearExpression.AddDecision((coef,d),expr) -> $"%f{coef} * {d.Name.AsString} + {doIt expr}"
          | LinearExpression.Multiply(coef,Empty) -> ""
          | LinearExpression.Multiply(1.,expr) -> doIt expr
          | LinearExpression.Multiply(coef,expr) -> $"%f{coef} * ({doIt expr})"
          | LinearExpression.AddFloat(k,Empty) -> $"%f{k}"
          | LinearExpression.AddFloat(0.,expr) -> doIt expr
          | LinearExpression.AddFloat(k,expr) -> $"%f{k} + ({doIt expr})"
          | LinearExpression.AddLinearExpression(l,Empty) -> $"{doIt l}"
          | LinearExpression.AddLinearExpression(Empty,r) -> $"{doIt r}"
          | LinearExpression.AddLinearExpression(l,r) -> $"{doIt l} + {doIt r}"
        doIt expr
      