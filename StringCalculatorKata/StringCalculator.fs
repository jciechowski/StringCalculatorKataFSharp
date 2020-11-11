module StringCalculatorKata

module StringCalculator =
    open System

    type Input = string
    type Delimiter = string
    type Numbers = int []
    type InputWithDelimiters = Input * Delimiter []

    let extractNumbers (numbers: Input) =
        let startsWithCustomDelimeter = numbers.StartsWith "//"

        match startsWithCustomDelimeter with
        | true -> numbers.[4..numbers.Length]
        | false -> numbers

    let extractDelimiter (numbers: Input) =
        let defaultDelimiters = [| ","; "\n" |]
        let startsWithCustomDelimeter = numbers.StartsWith "//"

        match startsWithCustomDelimeter with
        | true ->
            let customDelimiter = [| string numbers.[2] |]

            Array.concat [| defaultDelimiters
                            customDelimiter |]
        | false -> defaultDelimiters


    let parseInput (input: Input): InputWithDelimiters =
        let numbers = extractNumbers input
        let delimiters = extractDelimiter input
        (numbers, delimiters)

    let convertToNumbers (input: InputWithDelimiters): Numbers =
        let (inputNumbers, delimiters) = input

        let toArray = Array.create 1

        match String.length inputNumbers with
        | 0 -> 0 |> toArray
        | 1 -> inputNumbers |> int |> toArray
        | _ ->
            inputNumbers.Split(delimiters, StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int

    let sumNumbers = parseInput >> convertToNumbers >> Seq.sum

    let Add (numbers: string): int =
        sumNumbers numbers
