module Controller

open GameCore
open Model
open View
open Microsoft.Xna.Framework.Input

let advanceGame (runState : RunState) worldState =
    let elapsed = runState.elapsed
    match worldState with
    | _ when runState.WasJustPressed Keys.Escape -> None
    | None -> 
        Some Title
    | other -> other