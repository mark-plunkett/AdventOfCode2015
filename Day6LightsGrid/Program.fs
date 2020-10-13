open System

type Point = {
    X: int
    Y: int
}

let point x y = { X = x; Y = y }

[<EntryPoint>]
let main argv =

    let commandRegex = System.Text.RegularExpressions.Regex("([a-z ]+)(\d+),(\d+) \w+ (\d+),(\d+)")
    let parseCommand command =
        let result = commandRegex.Match(command).Groups
        let cmd, x1, y1, x2, y2 = result.[1].Value, int result.[2].Value, int result.[3].Value, int result.[4].Value, int result.[5].Value
        let parsed = 
            match cmd.Trim() with
            | "toggle" -> not 
            | "turn on" -> fun _ -> true 
            | "turn off" -> fun _ -> false 
            | _ -> failwith "unsupported command"
        parsed, (point x1 y1), (point x2 y2)

    let updateGrid (grid: bool[,]) p1 p2 f =
        for x in [p1.X..p2.X] do
            for y in [p1.Y..p2.Y] do
                grid.[x,y] <- f grid.[x,y]

    let runCommand grid (command, p1, p2) =
        updateGrid grid p1 p2 command

    let grid = Array2D.create 1000 1000 false
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (String.IsNullOrEmpty >> not)
    |> Seq.map parseCommand
    |> Seq.iter (runCommand grid)
    
    grid
    |> Array2D.map (fun b -> if b then 1 else 0)
    |> Seq.cast<int>
    |> Seq.sum
    |> printfn "%i"

    0
