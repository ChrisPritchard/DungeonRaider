module Controller

open GameCore
open Model
open Bsp
open Microsoft.Xna.Framework.Input

let (dungeonSize, leafSize, roomSize) = 60, 10, 5

let advanceGame (runState : RunState) worldState =
    match worldState with
    | _ when runState.WasJustPressed Keys.Escape -> None
    | _ when runState.WasJustPressed Keys.R ->
        dungeon dungeonSize leafSize roomSize |> MapView |> Some
    | None -> 
        dungeon dungeonSize leafSize roomSize |> MapView |> Some
    | other -> other