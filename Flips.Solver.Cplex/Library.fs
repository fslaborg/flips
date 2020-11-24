module Flips.Solvers.Cplex
open Flips.Types
open ILOG.CPLEX
open System.Collections.Generic
open ILOG.Concert

let cplex = new ILOG.CPLEX.Cplex()


let varDictionary = new Dictionary<Decision, INumVar>()

let createVariable ({Type = decisionType}) =
    match decisionType with
    | Boolean ->
        
        let i : INumVarBound  = null
        cplex.NumExpr()
        
    | Integer (lb, ub) -> solver.MakeIntVar(float lb, float ub, name)
    | Continuous (lb, ub) -> solver.MakeNumVar(float lb, float ub, name)


let internal modelDecisionsToCplexVariables (model: Flips.Model.Model) context =

    // 1. get all decisions
    let flipsDecisions =
        model.Constraints 
        |> Seq.map (fun c -> c.Expression.GetDecisions())
        |> Seq.concat
        |> HashSet
    
    // decision to numvar dictionary
    let flipsDecisionToCplexNumVar = 
        [for {Name=DecisionName(decisionName); Type=decisionType} as decision in flipsDecisions do
            match decisionType with
            | Boolean ->
                let boolVal = cplex.BoolVar()
                decision,boolVal :> INumVar
            | Integer(LowerBound=lb;UpperBound=ub)
            | Continuous(LowerBound=lb;UpperBound=ub) -> 
                let numVarType :NumVarType = NumVarType.Float
                let numVar = 
                    if context.obfuscateVarNames then 
                        cplex.NumVar(lb, ub, decisionType.ToCplex)
                    else 
                        cplex.NumVar(lb, ub, decisionType.ToCplex, decisionName)
                
                decision,numVar
        ]
        |> readOnlyDict

    // the reverse, numvar to decision dictionary
    let cplexNumVarToFlipsDecision =
        flipsDecisionToCplexNumVar 
        |> Seq.map (fun (KeyValue(a,b)) -> b,a)
        |> readOnlyDict

    {cplexNumVarToFlipsDecision=cplexNumVarToFlipsDecision; flipsDecisionToCplexNumVar=flipsDecisionToCplexNumVar}
    
let internal modelConstraintsToCplexConstraints (model: Flips.Model.Model) =
    for c in model.Constraints do
        

let addConstraints (constraints: Flips.Types.Constraint seq) =
    for c in constraints do
        let lhs = LinearExpression.Reduce c.Expression.LHS
        let operandCount = lhs.Coefficients.Count
        let operands = Array.zeroCreate (operandCount + 1) 
        operands.[0] <- cplex.NumExpr()
        
        for KeyValue(decision,op) in lhs.Coefficients do
            op.Value
        ()
    ()

let addDecisions (decisions: Flips.Types.Decision array) =
    solver.
    ()