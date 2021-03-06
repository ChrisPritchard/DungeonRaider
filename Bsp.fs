module Bsp

open Model
open Adjacency

type Range = Range of kind:TileKind option * x:int * y:int * width:int * height:int
type PartitionType = Vertical | Horizontal
type BspResult =
    | Leaf of Range
    | Partition of PartitionType * BspResult * BspResult

let random = new System.Random(1)

let rec bspRooms minLeafSize minRoomSize (Range (_, x, y, width, height)) = 
    let minPartitionSize = minLeafSize * 2
    let recBsp = Range >> bspRooms minLeafSize minRoomSize

    let roomIn minRoomSize (Range (_, x, y, width, height)) =
        let roomWidth = random.Next(minRoomSize, width - 1)
        let roomHeight = random.Next(minRoomSize, height - 1)
        let roomX = x + random.Next(1, width - roomWidth - 1)
        let roomY = y + random.Next(1, height - roomHeight - 1)
        Range (Some Room, roomX, roomY, roomWidth, roomHeight)

    let splitOnX () = 
        let wiggleRoom = width - minPartitionSize
        let mid = random.Next(minLeafSize, minLeafSize + wiggleRoom + 1)
        Partition (Vertical, 
            recBsp (None, x, y, mid, height), 
            recBsp (None, x + mid, y, width - mid, height))
    let splitOnY () =
        let wiggleRoom = height - minPartitionSize
        let mid = random.Next(minLeafSize, minLeafSize + wiggleRoom + 1)
        Partition (Horizontal, 
            recBsp (None, x, y, width, mid), 
            recBsp (None, x, y + mid, width, height - mid))

    if width <= minPartitionSize && height <= minPartitionSize then 
        Leaf <| roomIn minRoomSize (Range (None, x, y, width, height))
    else if width <= minPartitionSize then
        splitOnY ()
    else if height <= minPartitionSize then
        splitOnX ()
    else
        match random.NextDouble() with
        | n when n >= 0.5 ->
            splitOnX ()
        | _ ->
            splitOnY ()

let corridorAndDoors (x, y, w, h) length =
    if length <= 2 then
        Some ([Range (Some Corridor, x, y, w, h)], length)
    else
        let set = 
            seq {
                match w with
                    | 1 -> yield Range (Some Corridor, x, y + 1, w, h - 2)
                    | _ -> yield Range (Some Corridor, x + 1, y, w - 2, h)
                yield Range (Some Door, x, y, 1, 1)
                match w with
                    | 1 -> yield Range (Some Door, x, y + h - 1, 1, 1)
                    | _ -> yield Range (Some Door, x + w - 1, y, 1, 1)
            } |> Seq.toList
        Some (set, length)

let tryFindCorridorBetween partitionType (Range (kind1, x1, y1, w1, h1)) (Range (kind2, x2, y2, w2, h2)) =
    let isNotAbove = x1 + w1 <= x2 || x1 >= x2 + w2
    let isNotLeft = y1 + h1 <= y2 || y1 >= y2 + h2

    match partitionType with
    | _ when kind1 = Some Door || kind2 = Some Door -> None
    | Vertical when isNotLeft -> None // no overlap
    | Horizontal when isNotAbove -> None
    | Vertical ->
        let oy = max y1 y2
        let oh = (min (y1 + h1) (y2 + h2)) - oy
        let mid = oy + (oh / 2)
        let length = x2 - (x1 + w1)
        corridorAndDoors (x1 + w1, mid, length, 1) length
    | Horizontal ->
        let ox = max x1 x2
        let ow = (min (x1 + w1) (x2 + w2)) - ox
        let mid = ox + (ow / 2)
        let length = y2 - (y1 + h1)
        corridorAndDoors (mid, y1 + h1, 1, length) length

let pairs sequence1 sequence2 = 
    sequence1 |> Seq.collect (fun item1 -> sequence2 |> Seq.map (fun item2 -> (item1, item2)))

let rec joined minCorridorLength bspResult = 
    seq {
        match bspResult with
        | Leaf room -> yield room
        | Partition (partitionType, bspRes1, bspRes2) ->
            let spaces1 = joined minCorridorLength bspRes1
            let spaces2 = joined minCorridorLength bspRes2

            let corridor = 
                pairs spaces1 spaces2 
                |> Seq.map (fun (space1, space2) -> 
                    tryFindCorridorBetween partitionType space1 space2)
                |> Seq.choose id 
                |> Seq.filter (fun (_, length) -> length >= minCorridorLength)
                |> Seq.sortBy (fun (_, length) -> length) 
                |> Seq.tryHead

            yield! spaces1
            match corridor with Some (c, _) -> yield! c | _ -> ()
            yield! spaces2
    } |> Seq.toList

let stairs bspResult = 
    let rec crawler leftResult rightResult =
        match leftResult, rightResult with
        | (Leaf left), (Leaf right) -> left, right  
        | (Partition (_, left, _)), right -> crawler left right
        | left, (Partition (_, _, right)) -> crawler left right
    let (Range (_, sx,sy,_,_)), (Range (_, ex,ey,ew,eh)) = 
        match bspResult with
        | (Partition (_, left, right)) -> crawler left right
        | (Leaf room) -> room, room
    (sx, sy - 1), [
            yield (ex + ew - 2, ey + eh - 2)
            yield (ex + ew - 1, ey + eh - 2)
            yield (ex + ew - 1, ey + eh - 1)
            yield (ex + ew - 2, ey + eh - 1)
        ]

let dungeon maxSize minLeafSize minRoomSize minCorridorLength = 
    let rooms = bspRooms minLeafSize minRoomSize (Range (None, 0, 0, maxSize, maxSize))
    let stairsUp, stairsDown = stairs rooms
    let allOpen = joined minCorridorLength rooms
    
    let inRange (ox, oy) (Range (kindOption, x, y, width, height)) =
        match kindOption with
        | None -> false
        | Some _ ->
            ox >= x && oy >= y && ox < x + width && oy < y + height

    let tiles = 
        [0..maxSize - 1] |> List.collect (fun x -> 
        [0..maxSize - 1] |> List.map (fun y -> 
            match (x, y) with
            | p when p = stairsUp -> Tile (x, y, StairsUp, 0uy)
            | p when List.contains p stairsDown -> Tile (x, y, StairsDown (List.findIndex ((=) p) stairsDown), 0uy)
            | _ ->
                let inRange = List.tryFind (inRange (x, y)) allOpen
                let kind = 
                    match inRange with 
                    | Some (Range ((Some kind), _, _, _, _)) -> kind
                    | _ -> Block
                Tile (x, y, kind, 0uy)))

    tiles |> List.map (fun (Tile (x, y, kind, _)) -> 
        let adjacency = 
            match kind with 
            | Block -> getOpenAdjacency x y tiles
            | _ -> getClosedAdjacency x y tiles
        Tile (x, y, kind, adjacency))