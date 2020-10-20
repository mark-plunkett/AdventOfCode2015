open System
type Distance = int
type CityName = string
type Connection = {
    From: CityName
    To: CityName
    Distance: Distance
}

let parseConnection (s: string) =
    s.Split(" ")
    |> fun [|a; _; b; _; d|] ->
        [
            { From = a; To = b; Distance = int d }
            { From = b; To = a; Distance = int d }
        ]

let toRoutes (connMap: Map<CityName, Map<CityName, Distance>>) =
    let rec traverse (city::_ as route) unvisited =
        seq {
            match Set.isEmpty unvisited with
            | true -> yield route
            | _ -> 
                let conns = connMap.[city]
                let nextCities = Set.filter conns.ContainsKey unvisited
                for nextCity in nextCities do
                    yield! traverse (nextCity::route) (unvisited.Remove nextCity)
        }
    
    let cityNames = 
        connMap
        |> Map.toSeq
        |> Seq.map fst
        |> Set.ofSeq
    cityNames
    |> Seq.collect (fun start -> traverse [start] (cityNames.Remove start))

[<EntryPoint>]
let main argv =

    let part1F = Seq.min
    let part2F = Seq.max

    let connMap =
        Seq.initInfinite (fun _ -> Console.ReadLine())
        |> Seq.takeWhile (String.IsNullOrEmpty >> not)
        |> Seq.collect parseConnection
        |> Seq.fold (fun connMap conn -> 
            match Map.tryFind conn.From connMap with
            | None -> connMap.Add(conn.From, (Map.ofList [conn.To, conn.Distance]))
            | Some m -> connMap.Add(conn.From, (m.Add(conn.To, conn.Distance)))
        ) Map.empty
    connMap
    |> toRoutes
    |> Seq.map (fun route -> 
        route
        |> Seq.pairwise
        |> Seq.sumBy (fun (c1, c2) -> connMap.[c1].[c2])
    )
    |> part2F
    |> printfn "%i"

    0
