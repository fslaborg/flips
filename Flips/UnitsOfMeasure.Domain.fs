namespace Flips.UnitsOfMeasure

open Flips
open Flips.UnitsOfMeasure.Types

[<RequireQualifiedAccess>]
module Decision =

    let create<[<Measure>] 'Measure> decisionName decisionType =
        let d = Decision.create decisionName decisionType
        Decision<'Measure>.Value d

    let createBoolean<[<Measure>] 'Measure> decisionName =
        let d = Decision.createBoolean decisionName
        Decision<'Measure>.Value d

    let createInteger<[<Measure>] 'Measure> decisionName (lowerBound:float<'Measure>) (upperBound:float<'Measure>) =
        let d = Decision.createInteger decisionName (float lowerBound) (float upperBound)
        Decision<'Measure>.Value d

    let createContinuous<[<Measure>] 'Measure> decisionName (lowerBound:float<'Measure>) (upperBound:float<'Measure>) =
        let d = Decision.createContinuous decisionName (float lowerBound) (float upperBound)
        Decision<'Measure>.Value d


[<RequireQualifiedAccess>]
module Objective =
    
    let create objectiveName sense (Value expr:LinearExpression<'Measure>) =
        Objective.create objectiveName sense expr


[<RequireQualifiedAccess>]
module Solution =

    let getValues (s:Types.Solution) (m:Map<_,Decision<'Measure>>) =
        let getWithDefault _ (Decision.Value d:Decision<'Measure>) =
            match Map.tryFind d s.DecisionResults with
            | Some v -> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> v
            | None -> FSharp.Core.LanguagePrimitives.FloatWithMeasure<'Measure> 0.0

        m |> Map.map getWithDefault

[<AutoOpen>]
module Builders =
    
    open Flips.Types
    open Flips.UnitsOfMeasure.Types

    type DecisionBuilder<[<Measure>] 'Measure> (decisionSetPrefix:string) =

        let createDecision indices decisionType =
            let name = namer decisionSetPrefix indices
            let decision = Decision.create<'Measure> name decisionType
            indices, decision

        member this.Yield (decisionType:DecisionType<'Measure>) =
            match decisionType with
            | DecisionType.Boolean -> Flips.Types.DecisionType.Boolean
            | DecisionType.Integer (lb, ub) -> Flips.Types.DecisionType.Integer (float lb, float ub)
            | DecisionType.Continuous (lb, ub) -> Flips.Types.DecisionType.Continuous (float lb, float ub)

        member this.For(source:seq<'a>, body:'a -> 'b) =
            source |> Seq.map (fun x -> x, body x)

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.collect (fun (e, f) ->
            f |> Seq.collect (fun (f, g) ->
            g |> Seq.collect (fun (g, h) ->
            h |> Seq.map (fun (h, i) -> createDecision (a,b,c,d,e,f,g,h) i))))))))

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.collect (fun (e, f) ->
            f |> Seq.collect (fun (f, g) ->
            g |> Seq.map (fun (g, h) -> createDecision (a,b,c,d,e,f,g) h)))))))

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.collect (fun (r, f) ->
            f |> Seq.map (fun (f, g) -> createDecision (a,b,c,d,e,f) g))))))

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*seq<_*DecisionType>>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.collect (fun (d, e) ->
            e |> Seq.map (fun (e, f) -> createDecision (a,b,c,d,e) f)))))

        member this.Run(a:seq<_*seq<_*seq<_*seq<_*DecisionType>>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.collect (fun (c, d) ->
            d |> Seq.map (fun (d, e) -> createDecision (a,b,c,d) e))))

        member this.Run(a:seq<_*seq<_*seq<_*DecisionType>>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.collect (fun (b, c) ->
            c |> Seq.map (fun (c, d) -> createDecision (a,b,c) d)))

        member this.Run(a:seq<_*seq<_*DecisionType>>) =
            a |> Seq.collect (fun (a, b) ->
            b |> Seq.map (fun (b, c) -> createDecision (a, b) c))

        member this.Run(a:seq<_*DecisionType>) = 
            a |> Seq.map (fun (a, b) -> createDecision a b)


[<AutoOpen>]
module Sum =

  open Flips.SliceMap

  type internal Summer () =

    static member inline Sum (x:ISliceData<_,_>) : Flips.UnitsOfMeasure.Types.LinearExpression<_> =
      TryFind.sum x.Keys x.TryFind

    static member Sum (x:ISliceData<_,Flips.UnitsOfMeasure.Types.Decision<_>>) : Flips.UnitsOfMeasure.Types.LinearExpression<_> =
      let newTryFind = x.TryFind >> Option.map (fun v -> 1.0 * v)
      TryFind.sum x.Keys newTryFind


  let inline sum (x:ISliceData<'Key, 'Value>) : Flips.UnitsOfMeasure.Types.LinearExpression<_> =
      TryFind.sum x.Keys x.TryFind

  let inline sumAll< ^a, ^b when ^a: (static member Sum: ^a -> Flips.Types.LinearExpression) 
                            and ^a: (static member (+): ^a * ^a -> ^a)
                            and ^a: (static member Zero: ^a)> (k1: ^a seq) : Flips.Types.LinearExpression = 
      let r = Seq.sum k1
      ((^a) : (static member Sum: ^a -> Flips.Types.LinearExpression) r)