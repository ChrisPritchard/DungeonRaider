module Bsp

open Model

// what do i want to do here?
// 1. divide into a tree
// 2. join each parition
// - joining involves:
// a. determining type (horizontal or vertical) - this can be derived
// b. finding all ranges in each branch
// c. starting from the middle, find a corridor between that will connect two ranges without breaking rules
// cb. or, i could get the centre of each room, find the closest to the middle...this seems silly
// so how do I do this find algorithm?
// i. get the closest to the edge (maybe just sort)
// ii. run iterate out from the centre (both dirs) until the shortest corridor is found that links and follows rules
// might require a test to ensure no intersection

type Range = Range of x:int * y:int * width:int * height:int
type PartitionType = Vertical | Horizontal
type BspResult = 
    | Leaf of Range
    | Partition of PartitionType * BspResult * BspResult

let random = new System.Random()

let rec bspRooms minLeafSize minRoomSize (Range (x, y, width, height)) = 
    let minPartitionSize = minLeafSize * 2
    let recBsp = Range >> bspRooms minLeafSize minRoomSize
    let roomIn minRoomSize (Range (x, y, width, height)) =
        let roomWidth = random.Next(minRoomSize, width - 1)
        let roomHeight = random.Next(minRoomSize, height - 1)
        let roomX = x + random.Next(1, width - roomWidth - 1)
        let roomY = y + random.Next(1, height - roomHeight - 1)
        Range (roomX, roomY, roomWidth, roomHeight)

    let splitOnX () = 
        let wiggleRoom = width - minPartitionSize
        let mid = random.Next(minLeafSize, minLeafSize + wiggleRoom + 1)
        Partition (Vertical, recBsp (x, y, mid, height), recBsp (x + mid, y, width - mid, height))
    let splitOnY () =
        let wiggleRoom = height - minPartitionSize
        let mid = random.Next(minLeafSize, minLeafSize + wiggleRoom + 1)
        Partition (Horizontal, recBsp (x, y, width, mid), recBsp (x, y + mid, width, height - mid))

    if width <= minPartitionSize && height <= minPartitionSize then 
        Leaf <| roomIn minRoomSize (Range (x, y, width, height))
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

let corridorBetween partitionType (Range (x1, y1, w1, h1)) (Range (x2, y2, w2, h2)) =
    let isNotAbove = x1 + w1 <= x2 || x1 >= x2 + w2
    let isNotLeft = y1 + h1 <= y2 || y1 >= y2 + h2

    match partitionType with
    | Vertical when isNotLeft -> None // no overlap
    | Horizontal when isNotAbove -> None
    | Vertical ->
        let oy = max y1 y2
        let oh = (min (y1 + h1) (y2 + h2)) - oy
        let mid = oy + (oh / 2)
        let length = x2 - (x1 + w1)
        Some <| (Range (x1 + w1, mid, length, 1), length)
    | Horizontal ->
        let ox = max x1 x2
        let ow = (min (x1 + w1) (x2 + w2)) - ox
        let mid = ox + (ow / 2)
        let length = y2 - (y1 + h1)
        Some <| (Range (mid, y1 + h1, 1, length), length)

let rec joined bspResult = 
    seq {
        match bspResult with
        | Leaf room -> yield room
        | Partition (partitionType, bspRes1, bspRes2) ->
            let spaces1 = joined bspRes1
            let spaces2 = joined bspRes2

            let corridor = 
                spaces1 |> Seq.collect (fun space1 -> spaces2 |> Seq.map (fun space2 -> 
                    corridorBetween partitionType space1 space2))
                |> Seq.choose id |> Seq.sortBy (fun (_, length) -> length) |> Seq.tryHead

            yield! spaces1
            match corridor with Some (c, _) -> yield c | _ -> ()
            yield! spaces2
    } |> Seq.toList

let dungeon maxSize minLeafSize minRoomSize = 
    let rooms = bspRooms minLeafSize minRoomSize (Range (0, 0, maxSize, maxSize))
    let allOpen = joined rooms
    
    let inRange (ox, oy) (Range (x, y, width, height)) =
        ox >= x && oy >= y && ox < x + width && oy < y + height

    [0..maxSize - 1] |> List.collect (fun x -> 
    [0..maxSize - 1] |> List.map (fun y -> 
        let isWall = List.exists (inRange (x, y)) allOpen |> not
        Tile (x, y, isWall)))
    