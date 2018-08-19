module Controller

open GameCore
open Model
open Bsp
open View
open Microsoft.Xna.Framework.Input

let advanceGame (runState : RunState) worldState =
    let elapsed = runState.elapsed
    match worldState with
    | _ when runState.WasJustPressed Keys.Escape -> None
    | None -> 
        dungeon 50 10 8 |> MapView |> Some
    | other -> other