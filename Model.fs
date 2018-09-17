module Model

type Tile = Tile of x:int * y:int * kind:TileKind * adjacency:byte
and TileKind = Room | Door | Corridor | Block | StairsUp | StairsDown of int

type Entity = {
        state: EntityState
        facing: Facing
        position: int * int
    }
and EntityState =
    | Standing of startTime:float 
    | Gesturing of startTime:float 
    | Walking of startTime:float * path:(int * int) list
    | Striking of startTime:float * target:Entity
    | Hit of startTime:float
    | Dying of startTime:float 
    | Dead
and Facing = Left | Right

type GameModel = 
    | Playing of map:(Tile list) * player:Entity * monsters:(Entity list)