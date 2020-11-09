module Tests


open StringCalculatorKata
open StringCalculator
open Xunit

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

[<Theory>]
[<InlineData("1\n2", 3)>]
[<InlineData("5\n8", 13)>]
[<InlineData("1,2\n3,5\n8,13", 32)>]
let ``Passing numbers with a newline separator adds them`` (input, expected) =
    let result = Add input
    Assert.Equal(expected, result)

[<Theory>]
[<InlineData("//;\n1;2", 3)>]
[<InlineData("//.\n1.2", 3)>]
[<InlineData("//!\n1!2", 3)>]
let ``Passing numbers with a custom delimiter adds them correctly `` (input, expected) =
    let result = Add input
    Assert.Equal(expected, result)
