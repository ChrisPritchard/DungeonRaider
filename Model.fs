module Model

type Tile = Tile of x:int * y:int * kind:TileKind * adjacency:byte
and TileKind = Room | Door | Corridor | Block | StairsUp | StairsDown of int

type EntityState =
    | Standing of startTime:float 
    | Gesturing of startTime:float 
    | Walking of startTime:float 
    | Striking of startTime:float 
    | Dying of startTime:float 
    | Dead

type Facing = Left | Right

type Entity = {
        state: EntityState
        facing: Facing
        position: float * float
    }

type GameModel = 
    | Playing of map:(Tile list) * player:Entity