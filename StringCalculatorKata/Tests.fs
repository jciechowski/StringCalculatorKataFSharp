module Tests

open System
open Xunit


let Add (numbers: string): int =
    match numbers.Length with
    | 0 -> 0
    | 1 -> int numbers
    | _ ->
        numbers.Split([| ',' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int
        |> Seq.sum

[<Theory>]
[<InlineData("", 0)>]
[<InlineData("5", 5)>]
[<InlineData("17", 17)>]
let ``Passing single value returns the same value`` (input, expected) =
    let result = Add input
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("1,2", 3)>]
[<InlineData("5,8", 13)>]
[<InlineData("1,2,3,5,8,13", 32)>]
let ``Passing numbers with a seperator adds them`` (input, expected) =
    let result = Add input
    Assert.Equal(expected, result)
