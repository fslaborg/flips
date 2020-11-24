namespace Flips.SliceMap


type SMap<'Key, 'Value when 'Key : comparison and 'Value : equality> = SliceMap.SMap<'Key, 'Value>
type SMap2<'Key1, 'Key2, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Value : equality> = SliceMap.SMap2<'Key1, 'Key2, 'Value>
type SMap3<'Key1, 'Key2, 'Key3, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Value : equality> = SliceMap.SMap3<'Key1, 'Key2, 'Key3, 'Value>
type SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Value : equality> = SliceMap.SMap4<'Key1, 'Key2, 'Key3, 'Key4, 'Value>
type SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value when 'Key1 : comparison and 'Key2 : comparison and 'Key3 : comparison and 'Key4 : comparison and 'Key5 : comparison and 'Value : equality> = SliceMap.SMap5<'Key1, 'Key2, 'Key3, 'Key4, 'Key5, 'Value>
type SliceType<'a when 'a : comparison> = SliceMap.SliceType<'a>

type ISliceData<'Key, 'Value when 'Key : comparison and 'Value : equality> = SliceMap.ISliceData<'Key, 'Value>
//type TryFind<'Key, 'Value> = SliceMap.TryFind<'Key, 'Value>

module TryFind = SliceMap.TryFind
module SMap = SliceMap.SMap
module SMap2 = SliceMap.SMap2
module SMap3 = SliceMap.SMap3
module SMap4 = SliceMap.SMap4
module SMap5 = SliceMap.SMap5

[<AutoOpen>]
module Sum =

    /// <summary>A function which sums the values contained in a SliceMap</summary>
    /// <param name="x">An instance of ISliceData</param>
    /// <returns>A LinearExpression with a Unit of Measure</returns>
    let inline sum (x: ISliceData<'Key, 'Value>) : Flips.Types.LinearExpression =
        TryFind.sum x.Keys x.TryFind