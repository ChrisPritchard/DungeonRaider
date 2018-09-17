module Model

type Tile = Tile of x:int * y:int * kind:TileKind * adjacency:byte
and TileKind = Room | Door | Corridor | Block | StairsUp | StairsDown of int

type Entity = {
        state: EntityState
        facing: Facing
        position: int * int
        timeBetweenTiles: float
        health: int
        events: EntityEvent list
    }
and EntityState =
    | Standing of startTime:float 
    | Gesturing of startTime:float 
    | Walking of startTime:float * path:(int * int) list
    | Striking of startTime:float * target:Entity * hasHit:bool
    | Hit of startTime:float
    | Dying of startTime:float 
    | Dead
and Facing = Left | Right
and EntityEvent = Struck of Entity

type GameModel = 
    | Playing of map:(Tile list) * player:Entity * monsters:(Entity list)

let newRogue position =
    { 
        state = Standing 0.
        facing = Left
        position = position
        timeBetweenTiles = 250.
        health = 10
        events = [] 
    }

let newMinotaur position =
    {
        state = Standing 0.
        facing = Left
        position = position
        timeBetweenTiles = 350.
        health = 5
        events = []
    }