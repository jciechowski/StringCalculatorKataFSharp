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
    Assert.Equal(Ok expected, result)

[<Theory>]
[<InlineData("1,2", 3)>]
[<InlineData("5,8", 13)>]
[<InlineData("1,2,3,5,8,13", 32)>]
let ``Passing numbers with a seperator adds them`` (input, expected) =
    let result = Add input
    Assert.Equal(Ok expected, result)

[<Theory>]
[<InlineData("1\n2", 3)>]
[<InlineData("5\n8", 13)>]
[<InlineData("1,2\n3,5\n8,13", 32)>]
let ``Passing numbers with a newline separator adds them`` (input, expected) =
    let result = Add input
    Assert.Equal(Ok expected, result)

[<Theory>]
[<InlineData("//;\n1;2", 3)>]
[<InlineData("//.\n1.2", 3)>]
[<InlineData("//!\n1!2", 3)>]
let ``Passing numbers with a custom delimiter adds them correctly `` (input, expected) =
    let result = Add input
    Assert.Equal(Ok expected, result)

[<Theory>]
[<InlineData("1,-3", "-3")>]
[<InlineData("-3", "-3")>]
[<InlineData("0,-3", "-3")>]
[<InlineData("1,2,-3", "-3")>]
[<InlineData("1,2,-3, -4", "-3,-4")>]
let ``Cannot add negative values`` (input, expected) =
    let result = Add input
    Assert.Equal(Error expected, result)

[<Theory>]
[<InlineData("1000, 1", 1)>]
[<InlineData("1002, 1", 1)>]
[<InlineData("1001, 1000, 1", 1)>]
let ``Ignore values greater than 1000`` (input, expected) =
    let result = Add input
    Assert.Equal(Ok expected, result)
