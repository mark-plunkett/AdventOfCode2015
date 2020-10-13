open System

type Point = {
    X: int
    Y: int
}

let point x y = { X = x; Y = y }

let commandRegex = System.Text.RegularExpressions.Regex("([a-z ]+)(\d+),(\d+) \w+ (\d+),(\d+)")
let parseCommand command fToggle fTurnOn fTurnOff =
    let result = commandRegex.Match(command).Groups
    let cmd, x1, y1, x2, y2 = result.[1].Value, int result.[2].Value, int result.[3].Value, int result.[4].Value, int result.[5].Value
    let parsed = 
        match cmd.Trim() with
        | "toggle" -> fToggle 
        | "turn on" -> fTurnOn
        | "turn off" -> fTurnOff
        | _ -> failwith "unsupported command"
    parsed, (point x1 y1), (point x2 y2)

let updateGrid<'t> (grid: 't[,]) p1 p2 f =
    for x in [p1.X..p2.X] do
        for y in [p1.Y..p2.Y] do
            grid.[x,y] <- f grid.[x,y]

let runCommand grid (command, p1, p2) =
    updateGrid grid p1 p2 command

let buildGrid w h commands =
    let grid = Array2D.create w h 0 in Seq.iter (runCommand grid) |> ignore
    grid

module Part1 =

    let toggle i = if i = 0 then 1 else 0
    let turnOn _ = 1
    let turnOff _ = 0
    let parse command = parseCommand command toggle turnOn turnOff

module Part2 =

    let toggle i = i + 2
    let turnOn i = i + 1
    let turnOff i = max 0 (i - 1)
    let parse command = parseCommand command toggle turnOn turnOff

[<EntryPoint>]
let main argv =

    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (String.IsNullOrEmpty >> not)
    |> Seq.map Part2.parse
    |> buildGrid 1000 1000
    |> Seq.cast<int>
    |> Seq.sum
    |> printfn "%i"

    0
