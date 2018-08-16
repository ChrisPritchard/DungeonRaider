module Controller

open GameCore
open Model
open View
open Microsoft.Xna.Framework.Input

let getMap dim = 
    [
        Tile (0, 0, true); Tile (1, 0, true); Tile (2, 0, true)
        Tile (0, 1, true); Tile (1, 1, false); Tile (2, 1, true)
        Tile (0, 2, true); Tile (1, 2, true); Tile (2, 2, true)
    ]

let advanceGame (runState : RunState) worldState =
    let elapsed = runState.elapsed
    match worldState with
    | _ when runState.WasJustPressed Keys.Escape -> None
    | None -> 
        getMap 20 |> MapView |> Some
    | other -> other