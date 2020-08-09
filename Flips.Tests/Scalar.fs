namespace Flips.Tests

[<CustomEquality; CustomComparison>]
type Scalar = Value of float with

    static member private NearlyEquals (Value a:Scalar) (Value b:Scalar) : bool =
        let aValue = System.BitConverter.DoubleToInt64Bits a
        let bValue = System.BitConverter.DoubleToInt64Bits b
        if (aValue >>> 63) <> (bValue >>> 63) then
            a = b
        else
            System.Math.Abs(aValue - bValue) <= 10_000L

    static member (+) (Value lhs:Scalar, Value rhs:Scalar) =
        Value (lhs + rhs)

    static member (+) (Value s:Scalar, f:float) =
        Value (s + f)

    static member (+) (f:float, Value s:Scalar) =
        Value (s + f)

    static member (*) (Value lhs:Scalar, Value rhs:Scalar) =
        Value (lhs * rhs)

    static member (*) (Value s:Scalar, f:float) =
        Value (s * f)

    static member (*) (f:float, Value s:Scalar) =
        Value (s * f)

    static member (-) (Value lhs:Scalar, Value rhs:Scalar) =
        Value (lhs - rhs)

    static member (-) (Value s:Scalar, f:float) =
        Value (s - f)

    static member (-) (f:float, Value s:Scalar) =
        Value (f - s)

    static member (/) (Value lhs:Scalar, Value rhs:Scalar) =
        Value (lhs / rhs)

    static member (/) (f:float, Value s:Scalar) =
        Value (f / s)

    static member (/) (Value s:Scalar, f:float) =
        Value (s / f)

    static member Zero = Value 0.0

    override this.GetHashCode () =
        let (Value v) = this
        hash v

    override this.Equals(obj) =
        match obj with
        | :? Scalar as s -> Scalar.NearlyEquals this s 
        | _ -> false

    interface System.IComparable with
        member this.CompareTo yObj =
            match yObj with
            | :? Scalar as s -> compare this s
            | _ -> invalidArg "yObj" "Cannot compare values of different types"