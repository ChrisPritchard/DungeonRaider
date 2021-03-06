module Adjacency

open Model

let adjacencyKey = 
    [
        (-1,-1),128
        (0,-1),64
        (1,-1),32
        (1,0),16
        (1,1),8
        (0,1),4
        (-1,1),2
        (-1,0),1
    ] |> Map.ofList

let diagonals = [128;32;8;2] |> Set.ofList
let walls = [64;16;4;1] |> Set.ofList
let corners = [[128;64;1];[64;32;16];[16;8;4];[4;2;1]] |> List.map Set.ofList

let private getAdjacency x y tiles notCheck =
    let foldPoints = Set.fold ((|||)) 0 >> byte

    let adjacent = 
        tiles 
        |> List.filter 
            (fun (Tile (ox, oy, kind, _)) -> 
                if notCheck kind then false
                else 
                    abs (ox - x) <= 1 && abs (oy - y) <= 1 && not (ox = x && oy = y))
        |> List.map (fun (Tile (ox, oy, _, _)) -> ox - x, oy - y)
    let asByte point = adjacencyKey.Item point
    let keys =
        adjacent 
        |> List.map asByte
        |> Set.ofList
    if Set.difference keys diagonals |> Set.isEmpty then
        foldPoints keys
    else 
        match List.tryFindIndex (fun corner -> Set.intersect keys corner = corner) corners with
        | Some i -> foldPoints corners.[i]
        | _ -> Set.intersect keys walls |> foldPoints

let getOpenAdjacency x y tiles =
    getAdjacency x y tiles 
        (function | Block _ | StairsUp -> true | _ -> false) 

let getClosedAdjacency x y tiles =
    getAdjacency x y tiles 
        (function | Block _ | StairsUp | StairsDown _ -> false | _ -> true) 