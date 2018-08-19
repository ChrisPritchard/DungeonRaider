module Bsp

open Model

type Range = Range of x:int * y:int * width:int * height:int
type BspResult = 
    | Leaf of Range
    | Partition of BspResult * BspResult

let inRange (ox, oy) (Range (x, y, width, height)) =
    ox >= x && oy >= y && ox <= x + width && oy <= y + height

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
    let roomWidth = random.Next(minRoomSize, width - 2)
    let roomHeight = random.Next(minRoomSize, height - 2)
    let roomX = x + random.Next(1, width - roomWidth - 2)
    let roomY = y + random.Next(1, height - roomHeight - 2)
    Range (roomX, roomY, roomWidth, roomHeight)

let rec asRooms minRoomSize bspResult = 
    seq {
        match bspResult with
        | Leaf range -> yield roomIn range minRoomSize
        | Partition (bspRes1, bspRes2) -> 
            yield! asRooms minRoomSize bspRes1
            yield! asRooms minRoomSize bspRes2
    } |> Seq.toList

let dungeon maxSize minLeafSize minRoomSize = 
    let rooms = 
        bsp minLeafSize (Range (0, 0, maxSize, maxSize))
        |> asRooms minRoomSize
    [0..maxSize - 1] |> List.collect (fun x -> 
    [0..maxSize - 1] |> List.map (fun y -> 
        Tile (x, y, List.exists (inRange (x, y)) rooms |> not)))
    