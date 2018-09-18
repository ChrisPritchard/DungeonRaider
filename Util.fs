module Util

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