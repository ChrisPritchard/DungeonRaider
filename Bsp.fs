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

let corridorBetween (Range (x1, y1, w1, h1)) (Range (x2, y2, w2, h2)) =
    if x1 + w1 < x2 then
        // horizontal link
        let y = ((max y1 y2) + (min (y1 + h1) (y2 + h2))) / 2
        Range (x1 + w1, y, x2 - (x1 + w1), 1)
    else
        let x = ((max x1 x2) + (min (x1 + w1) (x2 + w2))) / 2
        Range (x, y1 + h1, 1, y2 - (y1 + h1))

let rec corridorsFor bspResult =
    seq {
        match bspResult with
        | Partition (Leaf room1, Leaf room2) -> 
            yield corridorBetween room1 room2
        | Leaf _ -> ()
        | Partition (bspRes1, bspRes2) -> 
            yield! corridorsFor bspRes1
            yield! corridorsFor bspRes2
    } |> Seq.toList

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
    let allOpen = List.concat [flattenRanges rooms; corridorsFor rooms]

    [0..maxSize - 1] |> List.collect (fun x -> 
    [0..maxSize - 1] |> List.map (fun y -> 
        let isWall = List.exists (inRange (x, y)) allOpen |> not
        Tile (x, y, isWall)))
    