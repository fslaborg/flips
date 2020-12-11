namespace Flips.Solver.Cplex.Internals

open System.Collections.Generic

open Flips.Types
open Flips.Types.TypeExtensions
open Flips.Types.TypeExtensions.Cplex

open ILOG.CPLEX
open ILOG.Concert

module rec CplexSolver =
    open System
        
    module Primitives =

        type Vars = IReadOnlyDictionary<DecisionName, INumVar>

        let setVariable  ({Type = decisionType; Name = DecisionName name }) (var:INumVar) =
            let lb,ub = 
                match decisionType with
                | Boolean -> 0.,1.
                | Integer (lb, ub) -> float lb, float ub
                | Continuous (lb, ub) -> lb,  ub
            if var.Type = decisionType.ToCplex then
                var.LB <- lb
                var.UB <- ub
            else
                failwithf "can't change type of decision %A from %A to %A" name decisionType var.Type

        let createVariable ({Type = decisionType; Name = DecisionName name }) state =

            let lb,ub = 
                match decisionType with
                | Boolean -> 0.,1.
                | Integer (lb, ub) -> float lb, float ub
                | Continuous (lb, ub) -> lb,  ub

            let variable = 
              if state.problem.toBackendSettings.obfuscateVarNames then
                state.cplex.NumVar(lb, ub, decisionType.ToCplex)
              else
                state.cplex.NumVar(lb, ub, decisionType.ToCplex, name)    
            
            variable    

        let buildExpression (vars: Vars) (expr: LinearExpression) state =
            let reducedExpr = Flips.Types.LinearExpression.Reduce expr
            let cplexLinearExpr = state.cplex.LinearNumExpr(Constant = reducedExpr.Offset)
            for KeyValue(decisionName, coeff) in reducedExpr.Coefficients do
                let var = vars.[decisionName]
                cplexLinearExpr.AddTerm(var, coeff)
            cplexLinearExpr

        let addObjective (vars: Vars) (objective: Flips.Types.Objective) state =
            let expr = buildExpression vars objective.Expression state
            
            match state.cplex.GetObjective() with
            | null ->
              let obfuscateNames = state.toCplexContext.obfuscateVarNames
              if obfuscateNames then
                state.cplex.AddObjective(objective.Sense.ToCplex, expr)
              else
                let (ObjectiveName(name)) = objective.Name
                state.cplex.AddObjective(objective.Sense.ToCplex, expr, name)
            | cplexObjective ->
                if state.options.HasDoNotReuseState then
                    failwithf "shouldn't reuse cplex state"
                else
                    cplexObjective.Expr <- expr
                    let obfuscateNames = state.toCplexContext.obfuscateVarNames
                    if not obfuscateNames then
                      let (ObjectiveName(name)) = objective.Name
                      cplexObjective.Name <- name
                    cplexObjective.Sense <- objective.Sense.ToCplex
                    cplexObjective

        let addConstraint (vars: Vars) (c: Flips.Types.Constraint) state =
            
            let lhs = buildExpression vars c.Expression.LHS state
            let rhs = buildExpression vars c.Expression.RHS state
            let obfuscateNames = state.toCplexContext.obfuscateConstraintNames 
            match c.Expression with
            | Equality (_, _)                  -> if obfuscateNames then state.cplex.AddEq(lhs, rhs) else state.cplex.AddEq(lhs, rhs, c.Name.AsString)
            | Inequality(_, GreaterOrEqual, _) -> if obfuscateNames then state.cplex.AddGe(lhs, rhs) else state.cplex.AddGe(lhs, rhs, c.Name.AsString)
            | Inequality(_, LessOrEqual, _)    -> if obfuscateNames then state.cplex.AddLe(lhs, rhs) else state.cplex.AddLe(lhs, rhs, c.Name.AsString)
        
        let addConstraints vars constraints state =
            [|
                for c in constraints do
                    addConstraint vars c state
            |]

        let buildSolution formulation state =
            let decisions = formulation.decisions.Values
            let decisionMap =
                decisions
                |> Seq.map (fun d ->
                    match formulation.vars.TryGetValue d.Name with
                    | true, v -> d, state.cplex.GetValue v 
                    | false, _ ->
                        if state.ofCplexContext.unknownDecisionTurnsToZero then
                            d, 0.
                        else
                            raise (System.Exception(sprintf "Decision %A was somehow not setup" d.Name))
                )
                |> readOnlyDict

            { new Flips.Solver.ISolution with 
                member x.Values = decisionMap }

        type CplexFormulation = 
            {
                decisions : IReadOnlyDictionary<DecisionName,Decision>
                vars: IReadOnlyDictionary<DecisionName,INumVar>
                constraints: IReadOnlyDictionary<Constraint, IConstraint>
                objective: Objective * IObjective
            }

        let formulateWithCplex (model: Flips.Types.Model) state =
            let decisionsByName = 
                [for d in model.GetDecisions() -> d]
                |> HashSet
                |> Seq.map (fun d -> d.Name, d)
                |> readOnlyDict
            
            let vars =
                match state.problem.flipsDecisionToCplexNumVar with
                | :? Dictionary<_,_> as d -> d
                | :? IReadOnlyDictionary<_,_> as d ->
                    let d =
                        [for (KeyValue(k,v)) in d -> k,v]
                        |> dict 
                        |> Dictionary
                    state.problem <- {state.problem with flipsDecisionToCplexNumVar = d }
                    d
                | _ ->
                    let d = Dictionary<_,_>(decisionsByName.Count)
                    state.problem <- {state.problem with flipsDecisionToCplexNumVar = d }
                    d
            
            for KeyValue(name,decision) in decisionsByName do
                match vars.TryGetValue name with
                | false, _ ->
                    vars.[name] <- 
                        let cplexVar = createVariable decision state
                        state.cplex.Add cplexVar |> ignore
                        cplexVar
                | true, var ->
                    if state.options.HasDoNotReuseState then
                        failwithf "using the same solver more than once is not an option"
                    else
                        printfn "variable %A already exists, removing before re-adding" name
                        setVariable decision var
            
            let cx = addConstraints vars model.Constraints state
            let cx = 
                Seq.zip model.Constraints cx
                |> readOnlyDict
            let objective = model.Objectives.Head
            
            let cplexObjective = addObjective vars objective state
            { decisions = decisionsByName
              vars = vars
              objective = objective, cplexObjective
              constraints = cx }

        let runSolverWithCallbacks (model: Flips.Types.Model) state (callbackHandler: Cplex.Callback seq) =
          
          state.cplex.ClearCallbacks()

          let cplexFormulation = formulateWithCplex model state
          
          if not (isNull callbackHandler) then
            for callback in callbackHandler do
              if not (isNull callback) then
                state.cplex.Use callback

          // look for WriteToFile option
          state.options.ExportToFiles 
          |> Seq.iter(state.cplex.ExportModel)

          use __ = 
            { new IDisposable with 
              member x.Dispose () =
                state.cplex.ClearCallbacks()
            } 

          if state.cplex.Solve() then
              let solution = buildSolution cplexFormulation state
              Ok (solution,cplexFormulation)
          else
              Error ""
          
        let runSolverWithCallback (model: Flips.Types.Model) state (callbackHandler: Cplex.Callback) =
            runSolverWithCallbacks model state [|callbackHandler|]

        let solve (model: Flips.Types.Model) state = runSolverWithCallback model state null
  
    type ICPlexOption =
        inherit System.IComparable

    [<CustomEquality;CustomComparison>]
    type NativeParam = 
      | NativeParam of param: ILOG.CPLEX.Cplex.Param
      override x.GetHashCode () =
        let (NativeParam p) = x 
        p.GetType().Name.GetHashCode() ^^^ p.GetValue()
      override x.Equals other =
        (x :> IComparable).CompareTo(other) = 0
      interface IComparable with
        member x.CompareTo other =
          if isNull other then
            1
          else
            match other with
            | :? NativeParam as other ->
              match other with
              | NativeParam other -> 
                let (NativeParam x) = x
                let nameCompare = x.GetType().Name.CompareTo(other.GetType().Name)
                if nameCompare = 0 then
                  x.GetValue().CompareTo(other.GetValue())
                else
                  nameCompare
            | _ -> 1
          
    type CplexOption =
        | DoNotReuseState
        | WriteToFile of fileName: string
        | CplexIntOption of Cplex.IntParam * value: int
        | CplexLongOption of Cplex.LongParam * value: int64
        | CplexBoolOption of Cplex.BooleanParam * value: bool
        | CplexDoubleOption of Cplex.DoubleParam * value: float
        | CplexStringOption of Cplex.StringParam * value: string
        
        member x.CompareTo (other: obj) = 
          if isNull other then 1 else
          match other with
          | :? CplexOption as other ->
            match other, x with
            | CplexIntOption(p,v), CplexIntOption(p2,v2) ->
              if p = p2 then v2.CompareTo v
              else p2.GetType().Name.CompareTo (p.GetType().Name)
            | CplexBoolOption(p,v), CplexBoolOption(p2,v2) ->
              if p = p2 then v2.CompareTo v
              else p2.GetType().Name.CompareTo (p.GetType().Name)
            | CplexDoubleOption(p,v), CplexDoubleOption(p2,v2) ->
              if p = p2 then v2.CompareTo v
              else p2.GetType().Name.CompareTo (p.GetType().Name)
            | CplexStringOption(p,v), CplexStringOption(p2,v2) ->
              if p = p2 then v2.CompareTo v
              else p2.GetType().Name.CompareTo (p.GetType().Name)
            | CplexLongOption(p,v), CplexLongOption(p2,v2) ->
              if p = p2 then v2.CompareTo v
              else p2.GetType().Name.CompareTo (p.GetType().Name)
            | _ -> 1
          | _ -> 1

        interface ICPlexOption with
          member x.CompareTo other =  x.CompareTo other


    type CplexOptions =
        {
            options : ICPlexOption Set
        }
        member x.HasDoNotReuseState = x.options.Contains DoNotReuseState
        member x.ExportToFiles =
            x.options 
            |> Set.toSeq
            |> Seq.choose (function | :? CplexOption as x -> Some x | _ -> None)
            |> Seq.choose (function WriteToFile file -> Some file | _ -> None)
            |> Seq.toArray

    type ICplexRunningSolverState = 
      internal {
        solverState: ICplexSolverState
        task: System.Threading.Tasks.Task
      }

    type State = 
      static member ofCplex (cplex: Cplex) = ICplexSolverState.createWithCplex cplex
      static member getCplex state         = state.cplex

    type ICplexSolverState =
        internal {
            cplex: Cplex
            mutable problem: CPlexProblemState
            lock: obj
            ofCplexContext: OfCplexContext
            toCplexContext: ToCplexContext
            options       : CplexOptions
        }

        member x.TryGetNumVar decisionName = x.problem.flipsDecisionToCplexNumVar.TryGetValue decisionName
        member x.TryGetConstraint constraintName = x.problem.flipsConstraintToCplexExpr.TryGetValue constraintName
        
        static member createWithCplex cplex =
          let event = new Event<_>()
          {
            cplex          = cplex
            problem        = createEmpty()
            lock           = obj()
            ofCplexContext = { unknownDecisionTurnsToZero = true }
            toCplexContext = { obfuscateConstraintNames = false; obfuscateVarNames = false }
            options        = { options =  Set.ofList [DoNotReuseState] }
          }
        
        static member create () = ICplexSolverState.createWithCplex(new Cplex())
              

        static member setOption cplexOption (state: ICplexSolverState) = 
            { state with options = { state.options with options = Set.add cplexOption state.options.options } }

        static member setCplexStringOption (value: string) (param: ILOG.CPLEX.Cplex.StringParam) (state: ICplexSolverState) = 
            state.cplex.SetParam(param, value)
            let option : ICPlexOption = CplexStringOption(param, value) :> _
            ICplexSolverState.setOption option state
        static member setCplexIntOption (param: ILOG.CPLEX.Cplex.IntParam) (value: int)  (state: ICplexSolverState) = 
            state.cplex.SetParam(param, value)
            let option : ICPlexOption = CplexIntOption(param, value) :> _
            ICplexSolverState.setOption option state
        static member setCplexLongOption (param: ILOG.CPLEX.Cplex.LongParam) (value: int64) (state: ICplexSolverState) = 
            state.cplex.SetParam(param, value)
            let option : ICPlexOption = CplexLongOption(param, value) :> _
            ICplexSolverState.setOption option state
        static member setCplexDoubleOption (param: ILOG.CPLEX.Cplex.DoubleParam) (value: float) (state: ICplexSolverState) = 
            state.cplex.SetParam(param, value)
            let option : ICPlexOption = CplexDoubleOption(param, value) :> _
            ICplexSolverState.setOption option state
        static member setCplexBoolOption (param: ILOG.CPLEX.Cplex.BooleanParam) (value: bool) (state: ICplexSolverState) = 
            state.cplex.SetParam(param, value)
            let option : ICPlexOption = CplexBoolOption(param, value) :> _
            
            ICplexSolverState.setOption option state

        /// remark: use removeCplexOptions if many
        static member removeCplexOption (o: ILOG.CPLEX.Cplex.Param) state =
          let otherValue = o.GetValue()
          let remaining = 
            [|
              for p in state.cplex.GetParameterSet() do
                match p with
                | :? ILOG.CPLEX.Cplex.Param as p ->
                  if p.GetValue () = otherValue then
                    ()
                | p ->
                  match p with
                  | :? Cplex.IntParam as p     -> if p.GetValue() = otherValue then () else p :> Cplex.Param, box (state.cplex.GetParam(p))
                  | :? Cplex.LongParam as p    -> if p.GetValue() = otherValue then () else p :> Cplex.Param, box (state.cplex.GetParam(p))
                  | :? Cplex.StringParam as p  -> if p.GetValue() = otherValue then () else p :> Cplex.Param, box (state.cplex.GetParam(p))
                  | :? Cplex.BooleanParam as p -> if p.GetValue() = otherValue then () else p :> Cplex.Param, box (state.cplex.GetParam(p))
                  | :? Cplex.DoubleParam as p  -> if p.GetValue() = otherValue then () else p :> Cplex.Param, box (state.cplex.GetParam(p))
                  | _ -> failwith $"can't get param value %A{p}"
                  ()
            |]
          state.cplex.GetParameterSet().Clear()
          for p,v in remaining do
            match p with
            | :? Cplex.IntParam as p     -> state.cplex.SetParam(p, unbox v)
            | :? Cplex.LongParam as p    -> state.cplex.SetParam(p, unbox v)
            | :? Cplex.StringParam as p  -> state.cplex.SetParam(p, unbox v)
            | :? Cplex.BooleanParam as p -> state.cplex.SetParam(p, unbox v)
            | :? Cplex.DoubleParam as p  -> state.cplex.SetParam(p, unbox v)
            | _ -> failwith $"can't set param value %A{p}"
          state
        static member removeOption (cplexOptionPredicate: CplexOption -> bool) state = 
            let newOptions =
                [
                    for i in state.options.options do
                        match i with
                        | :? CplexOption as i ->
                            if not (cplexOptionPredicate i) then
                                i :> ICPlexOption
                            else
                              match i with
                              | CplexIntOption(o,v) ->    failwith $"solver parameter can't be reset individually {o.GetType().Name}"
                              | CplexDoubleOption(o,v) -> failwith $"solver parameter can't be reset individually {o.GetType().Name}"
                              | CplexStringOption(o,v) -> failwith $"solver parameter can't be reset individually {o.GetType().Name}"
                              | CplexLongOption(o,v) ->   failwith $"solver parameter can't be reset individually {o.GetType().Name}"
                              | CplexBoolOption(o,v) ->   failwith $"solver parameter can't be reset individually {o.GetType().Name}"
                              | _ -> ()
                        | _ -> ()
                ] |> Set.ofList
            { state with options = { state.options with options = newOptions } }


    type CPlexProblemState = 
        {
            cplexObjectivesToFlips     : IReadOnlyDictionary<IObjective, ObjectiveName>
            cplexNumVarToFlipsDecision : IReadOnlyDictionary<INumVar,DecisionName>
            flipsDecisionToCplexNumVar : IReadOnlyDictionary<DecisionName,INumVar>
            flipsConstraintToCplexExpr : IReadOnlyDictionary<Constraint, IConstraint>
            cplexExprToFlipsConstraint : IReadOnlyDictionary<IConstraint,Constraint>
            toBackendSettings          : ToCplexContext
        } 
        static member empty = 
            {
                cplexObjectivesToFlips     = readOnlyDict []
                cplexNumVarToFlipsDecision = readOnlyDict []
                flipsDecisionToCplexNumVar = readOnlyDict []
                flipsConstraintToCplexExpr = readOnlyDict []
                cplexExprToFlipsConstraint = readOnlyDict []
                toBackendSettings = { obfuscateVarNames = false; obfuscateConstraintNames = false}
            }
        member x.GetBackendVariable (decision: Decision) = x.flipsDecisionToCplexNumVar.[decision.Name]
        member x.GetBackendConstraint (expr: Constraint) = x.flipsConstraintToCplexExpr.[expr]
        member x.GetFlipsDecision(backendVariable: INumVar) = x.cplexNumVarToFlipsDecision.[backendVariable]
        member x.GetFlipsConstraint (expr: IConstraint) = x.cplexExprToFlipsConstraint.[expr]
    let createEmpty () = CPlexProblemState.empty
    
    let createFromModel toBackendSettings =
      { CPlexProblemState.empty with
          cplexNumVarToFlipsDecision = new Dictionary<_,_>()
          flipsDecisionToCplexNumVar = new Dictionary<_,_>()
          flipsConstraintToCplexExpr = new Dictionary<_,_>()
          cplexExprToFlipsConstraint = new Dictionary<_,_>()
          toBackendSettings          = toBackendSettings
      }

    type ToCplexContext = {
        obfuscateVarNames        : bool
        obfuscateConstraintNames : bool
    }

    type OfCplexContext = {
        /// in Flips.Solver.Optano -> 
        ///    ```fsharp
        ///    match Dictionary.tryFind d.Name vars with
        ///    | Some var -> d, var.Value
        ///    | None -> d, 0.0
        ///    ```
        /// some user may prefer an exception
        unknownDecisionTurnsToZero : bool
    }
