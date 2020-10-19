open System

module Part1 =

    let hexRegex = System.Text.RegularExpressions.Regex("""\\x[0-9a-fA-F]{2}""")
    let stripEscapeChars (s: string) =
        let output = s.Trim('"').Replace("\\\\", "x").Replace("\\\"", "x")
        hexRegex.Replace(output, "x")

    let solve strings =
        let literalCharCount = Seq.sumBy String.length strings
        let escapedCharCount = Seq.sumBy (stripEscapeChars >> String.length) strings
        literalCharCount - escapedCharCount

module Part2 =

    let escapeEscapeChars (s: string) =
        s.Replace("\\", "\\\\").Replace("\"", "\\\"")
        |> sprintf "\"%s\""

    let solve strings =
        let literalCharCount = Seq.sumBy String.length strings
        let escapedCharCount = Seq.sumBy (escapeEscapeChars >> String.length) strings
        escapedCharCount - literalCharCount
        
[<EntryPoint>]
let main argv =

    let input =
        Seq.initInfinite (fun _ -> Console.ReadLine())
        |> Seq.takeWhile (String.IsNullOrEmpty >> not)
        |> Seq.toArray

    printfn "%i" (Part2.solve input)

    0
