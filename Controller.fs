module Controller

open GameCore
open Model
open Bsp
open View
open Microsoft.Xna.Framework.Input

let (dungeonSize, leafSize, roomSize) = 40, 8, 5

let leftKeys = [Keys.Left;Keys.A]
let rightKeys = [Keys.Right;Keys.D]
let walkKeys = [Keys.Left;Keys.Right;Keys.Up;Keys.Down;Keys.A;Keys.D;Keys.W;Keys.S]

let handleCharacterState (runState : RunState) state facing =
    let elapsed = runState.elapsed
    let animFinished start = elapsed - start >= 10. * frameSpeed
    let newState = 
        match state with
        | _ when runState.WasJustPressed Keys.R ->
            Standing elapsed
        | Dead -> 
            Dead
        | Dying start when animFinished start ->
            Dead
        | _ when runState.WasJustPressed Keys.X -> 
            Dying elapsed
        | Standing _ when runState.WasJustPressed Keys.C -> 
            Gesturing elapsed
        | Gesturing start when animFinished start ->
            Standing elapsed
        | Standing _ when runState.IsPressed Keys.F -> 
            Striking elapsed
        | Striking start when animFinished start ->
            Standing elapsed
        | Standing _ when runState.IsAnyPressed walkKeys -> 
            Walking elapsed
        | Walking _ when runState.IsAnyPressed walkKeys |> not -> 
            Standing elapsed
        | other -> other
    let newFacing = 
        match state with
        | Standing _ | Walking _ ->
            if runState.IsAnyPressed leftKeys then Left
            else if runState.IsAnyPressed rightKeys then Right
            else facing
        | _ -> facing
    CharacterRender (newState, newFacing) |> Some
    

let advanceGame (runState : RunState) worldState =
    match worldState with
    | _ when runState.WasJustPressed Keys.Escape -> None
    | Some (MapView _) when runState.WasJustPressed Keys.R ->
        dungeon dungeonSize leafSize roomSize |> MapView |> Some
    | None -> 
        dungeon dungeonSize leafSize roomSize |> MapView |> Some
    //| None -> CharacterRender (Standing 0., Left) |> Some
    | Some (CharacterRender (state, facing)) -> handleCharacterState runState state facing
    | other -> other