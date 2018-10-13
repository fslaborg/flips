module Flips.Types


type Vector =
    | Dense of float []


type Matrix =
    | Dense of float [,]

type RowPermutation =
    | Dense of int []


type Factorization =
    | DenseLU of float [,]
    | DensePLU of RowPermutation * float [,]




