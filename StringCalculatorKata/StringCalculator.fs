module StringCalculatorKata

module StringCalculator =
    open System

    let extractNumbers (numbers: string) =
        let startsWithCustomDelimeter = numbers.StartsWith "//"
        if (startsWithCustomDelimeter) then numbers.[4..numbers.Length] else numbers

    let extractDelimiter (numbers: string) =
        let defaultDelimiters = [| ","; "\n" |]
        let startsWithCustomDelimeter = numbers.StartsWith "//"

        if (startsWithCustomDelimeter) then
            let customDelimiter = [| string numbers.[2] |]

            Array.concat [| defaultDelimiters
                            customDelimiter |]
        else
            defaultDelimiters

    let sumNumbers (numbers: string, delimiters: string []) =
        match numbers.Length with
        | 0 -> 0
        | 1 -> int numbers
        | _ ->
            numbers.Split(delimiters, StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> Seq.sum

    let Add (numbers: string): int =
        (extractNumbers numbers, extractDelimiter numbers)
        |> sumNumbers
