module Util

open GameCore
open Constants
open Model

let distanceBetween (x1, y1) (x2, y2) =
    sqrt <| (float x2 - float x1)**2. + (float y2 - float y1)**2.

let getTile x y map = 
    if x < 0 || y < 0 || x >= dungeonSize || y >= dungeonSize then None
    else Some <| List.item (x * dungeonSize + y) map

let isOpen map x y =
    match getTile x y map with
    | None -> false
    | Some (Tile (_, _, kind, _)) ->
        match kind with
        | Room | Door | Corridor -> true
        | _ -> false

let isVisible (x, y, width, height) =
    x + width/2 > 0 
    && x - width/2 < screenWidth 
    && y > 0 
    && y - height < screenHeight


let renderRect (wx, wy) (width, height) = 
    if showGrid then
        wx - width / 2 + 1, wy - height + 1, width - 2, height - 2
    else
        wx - width / 2, wy - height, width, height



let worldPos (tx, ty) = tx * tilewidth, ty * tileheight

let currentWorldPos runState entity = 
    let wx, wy = entity.position |> worldPos
    match entity.state with
    | Walking (startTime, path) ->
        let timeBetweenTiles = entity.timeBetweenTiles
        let moveTime = (runState.elapsed - startTime) % timeBetweenTiles
        let nextPos = List.head path
        let distance = moveTime / timeBetweenTiles
        let nx, ny = nextPos |> worldPos
        let dx, dy = nx - wx, ny - wy
        wx + int (float dx * distance), wy + int (float dy * distance)
    | _ -> wx, wy