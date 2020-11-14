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

    let findNegativeValues (numbers: Numbers) =
        let negativeValues = Array.filter (fun x -> x < 0) numbers
        match Array.isEmpty negativeValues with
        | true -> Ok numbers
        | false -> Error negativeValues

    let sumNumbers = parseInput >> convertToNumbers >> Seq.sum

    let sum (numbersWithNegatives: Result<Numbers, int[]>) =
        match numbersWithNegatives with
        | Ok n -> Ok (Seq.sum n)
        | Error e -> e |> Array.map string |> String.concat "," |> Error

    let sumWithFilter = parseInput >> convertToNumbers >> findNegativeValues >> sum

    let Add (numbers: string): Result<int, string> =
        sumWithFilter numbers
