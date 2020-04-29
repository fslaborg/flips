module Tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open Gens

do Arb.register<Domain> () |> ignore

[<Fact>]
let ``My test`` () =
    Assert.True(true)

[<Property>]
let ``Addition works`` (x:int) (y:int) =
    Assert.Equal(x + y, y + x)

module LinearExpression =
    open Flips.Domain

    //let decisionGen : Gen<Decision> =
    //    gen {
    //        let decisionName = Arb.Default.NonEmptyString()
            
    //    }

    [<Property>]
    let ``Addition of Decisions is associative`` (d1:Decision) (d2:Decision) =

        let e1 = d1 + d2
        let e2 = d2 + d1
        Assert.Equal(e1, e2)
