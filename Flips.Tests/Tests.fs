module Flips.Tests

open Xunit
open FsCheck
open FsCheck.Xunit
open Flips.Gens

[<Properties(Arbitrary = [| typeof<Domain> |] )>]
module LinearExpression =
    open Flips.Domain

    [<Property>]
    let ``Addition of Decisions is associative`` (d1:Decision) (d2:Decision) =

        let e1 = d1 + d2
        let e2 = d2 + d1
        Assert.Equal(e1, e2)

    [<Property>]
    let ``Addition of Decisions is commutative`` (d1:Decision) (d2:Decision) (d3:Decision) =

        let e1 = d1 + (d2 + d3)
        let e2 = (d1 + d2) + d3
        Assert.Equal(e1, e2)