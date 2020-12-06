module StringCalculatorKata

module StringCalculator =
    open System

    type Input = string
    type Delimiter = string
    type Numbers = int []
    type InputWithDelimiters = Input * Delimiter []

    type DelimiterType =
        | Default
        | OneCharacterLong
        | MoreThanOneCharacterLong

    let findDelimiterType (numbers: Input) =
        match numbers.StartsWith "//" with
        | false -> Default
        | true ->
            match numbers.Contains "[" && numbers.Contains "]" with
            | true -> MoreThanOneCharacterLong
            | false -> OneCharacterLong

    let extractDelimiters (numbers: Input, delimiterType: DelimiterType) =
        let defaultDelimiters = [| ","; "\n" |]

        match delimiterType with
        | Default -> defaultDelimiters
        | OneCharacterLong ->
            let customDelimiter = [| string numbers.[2] |]

            Array.concat [| defaultDelimiters
                            customDelimiter |]
        | MoreThanOneCharacterLong ->
            let delimitersStart = numbers.IndexOf "["
            let delimitersEnd = numbers.IndexOf "\n" - 1
            let delimiters = numbers.[delimitersStart..delimitersEnd]

            let customDelimiters =
                delimiters.Split([| "["; "]" |], StringSplitOptions.RemoveEmptyEntries)

            Array.concat [| defaultDelimiters
                            customDelimiters |]

    let extractNumbers (numbers: Input, delimiterType: DelimiterType) =
        match delimiterType with
        | Default -> numbers
        | OneCharacterLong -> numbers.[4..numbers.Length]
        | MoreThanOneCharacterLong ->
            let delimiterEnd = numbers.LastIndexOf "]" + 2
            numbers.[delimiterEnd..numbers.Length]

    let parseInput (input: Input): InputWithDelimiters =
        let delimiterType = findDelimiterType input
        let numbers = extractNumbers (input, delimiterType)
        let delimiters = extractDelimiters (input, delimiterType)

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

    let sum (numbersWithNegatives: Result<Numbers, int []>) =
        match numbersWithNegatives with
        | Ok n -> Ok(Seq.sum n)
        | Error e ->
            e
            |> Array.map string
            |> String.concat ","
            |> Error

    let ignoreValuesGreaterThan1000 (numbers: Numbers) = Array.filter (fun x -> x < 1000) numbers

    let sumWithFilter =
        parseInput
        >> convertToNumbers
        >> ignoreValuesGreaterThan1000
        >> findNegativeValues
        >> sum

    let Add (numbers: string): Result<int, string> = sumWithFilter numbers
