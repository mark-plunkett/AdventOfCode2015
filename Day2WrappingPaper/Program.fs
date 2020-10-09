open System

[<EntryPoint>]
let main argv =
    let calcPaper () =
        Seq.initInfinite (fun _ -> Console.ReadLine())
        |> Seq.takeWhile (fun s -> String.IsNullOrEmpty(s) |> not)
        |> Seq.sumBy (fun s -> 
            let lwh = s.Split('x')
            let l, w, h = int lwh.[0], int lwh.[1], int lwh.[2]
            let a, b, c = l * w, w * h, h * l
            (2 * a) + (2 * b) + (2 * c) + List.min [a;b;c]
        )

    let calcRibbon () =
        Seq.initInfinite (fun _ -> Console.ReadLine())
        |> Seq.takeWhile (fun s -> String.IsNullOrEmpty(s) |> not)
        |> Seq.sumBy (fun s -> 
            let lwh = s.Split('x')
            let l, w, h = int lwh.[0], int lwh.[1], int lwh.[2]
            let shortestPerimeter = 
                [l;w;h]
                |> List.sort
                |> List.take 2
                |> List.map ((*) 2)
                |> List.sum
            shortestPerimeter + (l * w * h)
        )
        
    calcRibbon () |> printfn "%i"
    0
