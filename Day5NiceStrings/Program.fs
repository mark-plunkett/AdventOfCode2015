open System
let tee f a =
    f a
    a

module Part1 =
    let vowels = Set.ofList ['a';'e';'i';'o';'u']
    let containsThreeVowels (s: string) =
        s 
        |> Seq.scan (fun count c -> 
            if vowels.Contains(c) then count + 1
            else count
        ) 0
        |> Seq.contains 3

    let containsDoubleChar (s: string) =
        s
        |> Seq.pairwise
        |> Seq.exists (fun (a, b) -> a = b)

    let forbiddenStrings = Set.ofList ["ab";"cd";"pq";"xy"]
    let containsForbiddenString (s: string) =
        forbiddenStrings
        |> Set.exists s.Contains

    let isNiceString s =
        [containsThreeVowels; containsDoubleChar; containsForbiddenString >> not]
        |> List.forall (fun predicate -> predicate s)

module Part2 =
    let containsPair (s: string) =
        let pairs =
            s
            |> Seq.windowed 2
            |> Seq.map String
            |> Array.ofSeq
        let indexed =
            pairs
            |> Seq.indexed
        indexed
        |> Seq.exists (fun (index, pair) -> 
            pairs
            |> fun a -> a.[index+2..] 
            |> Array.contains pair
        )

    let containsXYX (s: string) =
        s
        |> Seq.windowed 3
        |> Seq.exists (fun a -> a.[0] = a.[2])

    let isNiceString s =
        [containsPair; containsXYX]
        |> List.forall (fun predicate -> predicate s)

[<EntryPoint>]
let main argv =
    let countNiceStrings predicate strings =
        strings
        |> Seq.filter predicate
        |> Seq.length
   
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (String.IsNullOrEmpty >> not)
    |> countNiceStrings Part2.isNiceString
    |> printfn "%i"
    
    0