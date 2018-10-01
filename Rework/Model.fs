module Model

type PartitionType = Vertical | Horizontal

type BspResult =
    | Leaf of x:int * y:int * width:int * height:int
    | Partition of PartitionType * BspResult * BspResult

type TileKind = Room | Door | Corridor | Block | StairsUp | StairsDown of int

type GeneratorConfig = {
    width: int
    height: int
    minLeafSize: int * int
    minRoomSize: int * int
    minCorridorLength: int
}

type Dungeon = {
    rooms: (int * int * int * int) list
    corridors: (int * int * int * int) list
    tileTypes: Map<int * int, TileKind>
    adjacencies: Map<int * int, byte>
}
with 
    member __.Flattened () =
        (__.rooms @ __.corridors)
            |> List.collect (fun (x, y, width, height) ->
                [x..x+width] |> List.collect (fun tx -> 
                [y..y+height] |> List.map (fun ty -> 
                    (tx, ty, __.tileTypes.[tx, ty], __.adjacencies.[tx, ty]))))