open Model

[<EntryPoint>]
let main _ =
    
    let maxsize = 40
    let dungeon = Bsp.dungeon maxsize 6 4 2
    let map = dungeon |> Seq.map (fun (x, y, kind, _) -> (x, y), kind) |> Map.ofSeq

    [0..maxsize-1] |> List.iter (fun y ->
        let line =
            [0..maxsize-1] |> Seq.map (fun x ->
                match map.[x, y] with
                | Block -> "▓"
                | Room -> "░"
                | Corridor -> "▒"
                | Door -> "@"
                | StairsUp -> "0"
                | StairsDown n -> string n)
            |> String.concat ""
        printfn "%s" line)

    0