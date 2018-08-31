module Model

type Tile = Tile of x:int * y:int * kind:TileKind * adjacency:byte
and TileKind = Room | Door | Corridor | Block

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