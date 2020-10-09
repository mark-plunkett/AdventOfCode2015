open System

[<EntryPoint>]
let main argv =
    use md5 = System.Security.Cryptography.MD5.Create()
    let hash (s: string) = s |> Text.Encoding.UTF8.GetBytes |> md5.ComputeHash 
    let byteToHex (b: byte) = b.ToString("X2") 
    let bytesToHex = Seq.map byteToHex >> String.concat String.Empty
    
    let prefix = argv.[0]
    let input = Console.ReadLine()
    Seq.initInfinite id
    |> Seq.tryFind (sprintf "%s%i" input >> hash >> bytesToHex >> fun hex -> hex.StartsWith(prefix))
    |> function 
        | Some n -> printfn "%i" n
        | None -> printfn "Not found for %s" input
    
    0