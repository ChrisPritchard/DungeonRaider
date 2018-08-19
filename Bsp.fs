module Bsp

open Model

type Range = Range of x:int * y:int * width:int * height:int
type BspResult = 
    | Leaf of Range
    | Partition of BspResult * BspResult

let inRange (ox, oy) (Range (x, y, width, height)) =
    ox >= x && oy >= y && ox < x + width && oy < y + height

let centre (Range (x, y, width, height)) = 
    (x + (width / 2)), (y + (height / 2))

let random = new System.Random()

let rec bsp minLeafSize (Range (x, y, width, height)) = 
    let minPartitionSize = minLeafSize * 2
    let recBsp = Range >> bsp minLeafSize

    let splitOnX () = 
        let wiggleRoom = width - minPartitionSize
        let mid = random.Next(minLeafSize, minLeafSize + wiggleRoom + 1)
        Partition (recBsp (x, y, mid, height), recBsp (x + mid, y, width - mid, height))
    let splitOnY () =
        let wiggleRoom = height - minPartitionSize
        let mid = random.Next(minLeafSize, minLeafSize + wiggleRoom + 1)
        Partition (recBsp (x, y, width, mid), recBsp (x, y + mid, width, height - mid))

    if width <= minPartitionSize && height <= minPartitionSize then 
        Leaf (Range (x, y, width, height))
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

let roomIn (Range (x, y, width, height)) minRoomSize =
    let roomWidth = random.Next(minRoomSize, width - 1)
    let roomHeight = random.Next(minRoomSize, height - 1)
    let roomX = x + random.Next(1, width - roomWidth - 1)
    let roomY = y + random.Next(1, height - roomHeight - 1)
    Range (roomX, roomY, roomWidth, roomHeight)

let rec asRooms minRoomSize bspResult = 
    seq {
        match bspResult with
        | Leaf range -> yield roomIn range minRoomSize
        | Partition (bspRes1, bspRes2) -> 
            yield! asRooms minRoomSize bspRes1
            yield! asRooms minRoomSize bspRes2
    } |> Seq.toList

let rec totalRange =
    function
    | Leaf range -> range
    | Partition (bspRes1, bspRes2) ->
        let (Range (x1, y1, width1, height1)) = totalRange bspRes1
        let (Range (x2, y2, width2, height2)) = totalRange bspRes2
        let x = min x1 x2
        let y = min y1 y2
        let width = if x1 = x2 then width1 else width1 + width2
        let height = if y1 = y2 then height1 else height1 + height2
        Range (x, y, width, height)

let corridorBetween bspResult1 bspResult2 =
    let (x1, y1) = totalRange bspResult1 |> centre
    let (x2, y2) = totalRange bspResult2 |> centre
    if x1 = x2 then
        if y1 < y2 then Range (x1, y1, 1, y2 - y1)
        else Range (x1, y2, 1, y1 - y2)
    else
        if x1 < x2 then Range (x1, y1, x2 - x1, 1)
        else Range (x2, y1, x1 - x2, 1)

let rec asCorridors bspResult = 
    seq {
        match bspResult with
        | Leaf _ -> ()
        | Partition (bspRes1, bspRes2) ->
            yield corridorBetween bspRes1 bspRes2
            yield! asCorridors bspRes1
            yield! asCorridors bspRes2
    } |> Seq.toList

let dungeon maxSize minLeafSize minRoomSize = 
    let partitions = bsp minLeafSize (Range (0, 0, maxSize, maxSize))
    let rooms = partitions |> asRooms minRoomSize
    let corridors = partitions |> asCorridors
    [0..maxSize - 1] |> List.collect (fun x -> 
    [0..maxSize - 1] |> List.map (fun y -> 
        let isWall = 
            List.exists (inRange (x, y)) rooms |> not
            && List.exists (inRange (x, y)) corridors |> not
        Tile (x, y, isWall)))
    