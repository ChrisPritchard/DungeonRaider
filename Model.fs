module Model

type Tile = Tile of x:int * y:int * kind:TileKind
and TileKind = Room | Door | Corridor | Wall

type GameModel = 
    | MapView of Tile list
    | CharacterRender