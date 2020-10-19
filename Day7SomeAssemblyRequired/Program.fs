open System
open System.Text.RegularExpressions
type WireName = string
type WireValue = uint16
type Gate =
| And of (Source * Source)
| Or of (Source * Source)
| Not of Source
| LShift of (Source * int)
| RShift of (Source * int)

and Source =
| Value of WireValue
| Gate of Gate
| Wire of WireName

type Command = {
    Source: Source
    Destination: WireName
}

let tryMatch input regex f =
    let m = Regex.Match(input, regex)
    if m.Success then Some (m.Groups.Keys |> Seq.map (fun (k: string) -> k, m.Groups.[k].Value) |> Map.ofSeq |> f)
    else None

let rec parseGate (s: string) =
    [
        "NOT (?<gate>\w+)", fun map -> Not (Map.find "gate" map |> parseSource)
        "(?<gate1>\w+) AND (?<gate2>\w+)", fun map -> And (Map.find "gate1" map |> parseSource, Map.find "gate2" map |> parseSource)
        "(?<gate1>\w+) OR (?<gate2>\w+)", fun map -> Or (Map.find "gate1" map |> parseSource, Map.find "gate2" map |> parseSource)
        "(?<wire>\w+) LSHIFT (?<value>\w+)", fun map -> LShift (Map.find "wire" map |> parseSource, Map.find "value" map |> int)
        "(?<wire>\w+) RSHIFT (?<value>\w+)", fun map -> RShift (Map.find "wire" map |> parseSource, Map.find "value" map |> int)
    ]
    |> Seq.tryPick (fun (regex, f) -> tryMatch s regex f)

and parseSource (s: string) =
    match UInt16.TryParse(s) with
    | true, i -> Value i
    | _ -> 
        match parseGate s with
        | Some g -> Gate g
        | _ -> Wire s

let parseCommand (c:string) =
    let split = c.Split("->") |> Array.map (fun s -> s.Trim())
    let source, wire = parseSource <| split.[0], split.[1]
    { Source = source; Destination = wire }

let evalCommand commandMap c =
    let cache = System.Collections.Generic.Dictionary<string, uint16>()
    let rec evalCommandRec command =
        match cache.TryGetValue(command.Destination) with
        | true, v -> v
        | _ ->
            match command.Source with
            | Value v ->
                cache.TryAdd(command.Destination, v) |> ignore
                v
            | Wire w -> 
                let v = commandMap |> Map.find w |> evalCommandRec
                cache.TryAdd(command.Destination, v) |> ignore
                v
            | Gate g ->
                let v =
                    match g with
                    | And (s1, s2) -> (getGateArg s1) &&& (getGateArg s2)
                    | Or (s1, s2) -> (getGateArg s1) ||| (getGateArg s2)
                    | Not (s) -> ~~~ (getGateArg s)
                    | LShift (s, shift) -> (getGateArg s) <<< shift
                    | RShift (s, shift) -> (getGateArg s) >>> shift
                cache.TryAdd(command.Destination, v) |> ignore
                v
                
    and getGateArg source =
        match source with
        | Value v -> v
        | Wire w -> commandMap |> Map.find w |> evalCommandRec 
        | _ -> failwith "not supported"
    
    commandMap
    |> Map.find c.Destination
    |> evalCommandRec

let eval initialWire commands =
    let commandMap = commands |> Seq.map (fun c -> (c.Destination, c)) |> Map.ofSeq
    evalCommand commandMap (Map.find initialWire commandMap)

[<EntryPoint>]
let main argv =
    let wireName = argv.[0]
    Seq.initInfinite (fun _ -> Console.ReadLine())
    |> Seq.takeWhile (String.IsNullOrEmpty >> not)
    |> Seq.map parseCommand
    |> eval wireName
    |> printfn "%i"

    0
