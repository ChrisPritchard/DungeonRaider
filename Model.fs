module Model

open Constants

type Tile = Tile of x:int * y:int * kind:TileKind * adjacency:byte
and TileKind = Room | Door | Corridor | Block | StairsUp | StairsDown of int

type Entity = {
        kind: EntityKind
        state: EntityState
        facing: Facing
        position: int * int
        health: int
        events: EntityEvent list

        timeBetweenTiles: float
        size: int * int
        origin: int * int
    }
and EntityKind = Rogue | Minotaur | Skeleton
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
        kind = Rogue
        state = Standing 0.
        facing = Left
        position = position
        health = 10
        events = [] 
        timeBetweenTiles = 250.
        size = playerwidth, playerheight
        origin = position
    }

let newMinotaur position =
    {
        kind = Minotaur
        state = Standing 0.
        facing = Left
        position = position
        timeBetweenTiles = 350.
        health = 5
        events = []
        size = playerwidth * 3/2, playerheight * 3/2
        origin = position
    }

let newSkeleton position =
    {
        kind = Skeleton
        state = Standing 0.
        facing = Left
        position = position
        timeBetweenTiles = 350.
        health = 2
        events = []
        size = playerwidth, playerheight
        origin = position
    }