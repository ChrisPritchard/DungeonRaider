module Model

type Tile = Tile of x:int * y:int * kind:TileKind
and TileKind = Room | Door | Corridor | Wall of edges:byte

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

type CharacterState =
    | Standing of startTime:float 
    | Gesturing of startTime:float 
    | Walking of startTime:float 
    | Striking of startTime:float 
    | Dying of startTime:float 
    | Dead

type Facing = Left | Right

type GameModel = 
    | MapView of Tile list
    | CharacterRender of CharacterState * Facing