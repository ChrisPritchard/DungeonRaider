module Controller

open GameCore
open Model
open Bsp
open View
open Microsoft.Xna.Framework.Input

let (dungeonSize, leafSize, roomSize) = 60, 10, 5

let walkKeys = [Keys.Left;Keys.Right;Keys.Up;Keys.Down;Keys.A;Keys.D;Keys.W;Keys.S]

let handleCharacterState (runState : RunState) state =
    let elapsed = runState.elapsed
    let newState = 
        match state with
        | _ when runState.WasJustPressed Keys.R ->
            Standing elapsed
        | Dead -> 
            Dead
        | Dying start when elapsed - start >= 10. * frameSpeed ->
            Dead
        | _ when runState.WasJustPressed Keys.X -> 
            Dying elapsed
        | Standing _ when runState.WasJustPressed Keys.C -> 
            Gesturing elapsed
        | Gesturing start when elapsed - start >= 10. * frameSpeed ->
            Standing elapsed
        | Standing _ when runState.WasJustPressed Keys.F -> 
            Striking elapsed
        | Striking start when elapsed - start >= 10. * frameSpeed ->
            Standing elapsed
        | Standing _ when runState.IsAnyPressed walkKeys -> 
            Walking elapsed
        | Walking _ when runState.IsAnyPressed walkKeys |> not -> 
            Standing elapsed
        | other -> other
    CharacterRender newState |> Some
    

let advanceGame (runState : RunState) worldState =
    match worldState with
    | _ when runState.WasJustPressed Keys.Escape -> None
    | Some (MapView _) when runState.WasJustPressed Keys.R ->
        dungeon dungeonSize leafSize roomSize |> MapView |> Some
    // | None -> 
    //     dungeon dungeonSize leafSize roomSize |> MapView |> Some
    | None -> CharacterRender (Standing 0.) |> Some
    | Some (CharacterRender state) -> handleCharacterState runState state
    | other -> other