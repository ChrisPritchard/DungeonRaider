module Bsp

open Model

type Range = Range of x:int * y:int * width:int * height:int
type BspResult = 
    | Leaf of Range
    | Partition of BspResult * BspResult

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
    match bspResult with
    | Leaf range -> Leaf <| roomIn range minRoomSize
    | Partition (bspRes1, bspRes2) -> 
        Partition (asRooms minRoomSize bspRes1, asRooms minRoomSize bspRes2)

let rec flattenRanges bspResult = 
    seq {
        match bspResult with
        | Leaf range -> yield range
        | Partition (bspRes1, bspRes2) ->
            yield! flattenRanges bspRes1
            yield! flattenRanges bspRes2
    } |> Seq.toList

let inRange (ox, oy) (Range (x, y, width, height)) =
    ox >= x && oy >= y && ox < x + width && oy < y + height

let dungeon maxSize minLeafSize minRoomSize = 
    let partitions = bsp minLeafSize (Range (0, 0, maxSize, maxSize))
    let rooms = asRooms minRoomSize partitions
    //let corridors = corridorsFor rooms

    let allOpen = flattenRanges rooms

    [0..maxSize - 1] |> List.collect (fun x -> 
    [0..maxSize - 1] |> List.map (fun y -> 
        let isWall = List.exists (inRange (x, y)) allOpen |> not
        Tile (x, y, isWall)))
    