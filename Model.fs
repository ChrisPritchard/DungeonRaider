module Model

type Tile = Tile of x:int * y:int * wall:bool

type GameModel = 
    | MapView of Tile list