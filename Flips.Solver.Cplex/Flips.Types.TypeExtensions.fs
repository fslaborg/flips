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
[<AutoOpen>]
module Extensions =

  /// todo for matthewcrews: review if correct and can go to Flips.Solver
  type Flips.Types.LinearExpression with
      member internal x.GetDecisions() =
          seq {
              match x with
              | Empty -> ()
              | AddFloat(coeff, expr) -> ()
              | AddDecision((_,decision),expr) -> 
                  yield decision
                  yield! expr.GetDecisions()
              | Multiply(coeff,expr) -> 
                  yield! expr.GetDecisions()
              | AddLinearExpression(left,right) ->
                  yield! left.GetDecisions()
                  yield! right.GetDecisions()
          }

  /// todo for matthewcrews: review if correct and can go to Flips.Solver
  type Flips.Types.ConstraintExpression with
      member internal x.LHS =
          match x with
          | ConstraintExpression.Inequality(LHS=lhs)
          | ConstraintExpression.Equality(LHS=lhs) -> lhs
      member internal x.RHS =
          match x with
          | ConstraintExpression.Inequality(RHS=rhs)
          | ConstraintExpression.Equality(RHS=rhs) -> rhs

      member internal x.GetDecisions () =
          seq {
              match x with
              | ConstraintExpression.Equality(lhs,rhs) 
              | ConstraintExpression.Inequality(lhs,_,rhs) ->
                  yield! lhs.GetDecisions()
                  yield! rhs.GetDecisions()            
          }
