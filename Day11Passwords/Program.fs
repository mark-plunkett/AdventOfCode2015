open System

let nextChar c =
    match c with
    | 'z' -> 'a'
    | _ -> 
        let charValue = uint16 c
        charValue + 1us |> char

let wasRollover current next =
    current = 'z' && next = 'a'

let rotate (input: string) =
    ([], true)
    |> Seq.foldBack (fun currentChar acc ->
        let chars, shouldRollover = acc
        let nextChar = if shouldRollover then nextChar currentChar else currentChar
        nextChar::chars, wasRollover currentChar nextChar
    ) input
    |> (fst >> List.toArray >> String)
    
let containsConsecutiveChars (input: string) =
    input
    |> Seq.map uint16
    |> Seq.windowed 3
    |> Seq.exists (fun [|a;b;c|] -> b = a + 1us && c = a + 2us)

let forbiddenChars = Set.ofList ['i';'o';'l'] 
let containsForbiddenChars (input: string) =
    input
    |> Seq.exists (forbiddenChars.Contains)

let containsTwoNonOverlappingPairs (input: string) =
    input
    |> Seq.windowed 2
    |> Seq.filter (fun [|a;b|] -> a = b)
    |> Seq.distinct
    |> Seq.length
    |> fun len -> len > 1

[<EntryPoint>]
let main argv =
    let input = Console.ReadLine()
    let isValid password =
        containsConsecutiveChars password
        && (containsForbiddenChars password |> not)
        && containsTwoNonOverlappingPairs password
    
    Seq.initInfinite id
    |> Seq.scan (fun password _ -> rotate password) (rotate input)
    |> Seq.find isValid
    |> printfn "%s"

    0
