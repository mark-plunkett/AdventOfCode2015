open System

let countGroups (s: string) =
    let group (remaining: string) =
        match remaining with
        | "" -> None
        | _ -> 
            remaining
            |> Seq.takeWhile (fun x -> x = remaining.[0])
            |> Seq.length
            |> fun count -> Some ((count, remaining.[0]), remaining.[count..])

    Seq.unfold group s

let countGroupsFast (s: string) =
    s
    |> Seq.fold (fun acc char ->
        match acc with
        | (count, char')::tail when char = char' -> (count + 1, char')::tail
        | _ -> (1, char)::acc
    ) []
    |> List.rev

[<EntryPoint>]
let main argv =
    let iterations = int argv.[0]
    let input = Console.ReadLine()
    [1..iterations]
    |> Seq.fold (fun acc _ ->
        acc
        |> countGroupsFast
        |> Seq.map (fun (count, char) -> sprintf "%i%c" count char)
        |> String.concat String.Empty) input
    |> fun s -> printfn "%i" s.Length

    0
