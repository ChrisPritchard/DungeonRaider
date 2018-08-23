module Controller

open GameCore
open Model
open Bsp
open Microsoft.Xna.Framework.Input

let advanceGame (runState : RunState) worldState =
    match worldState with
    | _ when runState.WasJustPressed Keys.Escape -> None
    | None -> 
        dungeon 50 10 5 |> MapView |> Some
    | other -> other