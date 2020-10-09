open System

[<EntryPoint>]
let main argv =
    
    let folder currentPos direction =
        let x, y = currentPos
        match direction with
        | '^' -> x, y + 1
        | 'v' -> x, y - 1
        | '>' -> x + 1, y
        | '<' -> x - 1, y

    let calcNumDeliveries input =
        input
        |> Seq.scan folder (0, 0)
        |> Seq.groupBy id
        |> Seq.filter (fun (_, points) -> Seq.length points > 0)
        |> Seq.length
    
    let calcNumRoboDeliveries input =
        input
        |> Seq.toList
        |> List.mapi (fun i direction -> i, direction)
        |> List.partition (fun (i, direction) -> i % 2 = 0)
        |> (fun (santa, robo) ->
            Seq.append 
                (Seq.scan folder (0, 0) (santa |> List.map snd))
                (Seq.scan folder (0, 0) (robo |> List.map snd))
        )
        |> Seq.groupBy id
        |> Seq.filter (fun (_, points) -> Seq.length points > 0)
        |> Seq.length

    let input = Console.ReadLine()
    printfn "%A" <| calcNumRoboDeliveries input

    0
