module Controller

open GameCore
open Model
open Bsp
open View
open Microsoft.Xna.Framework.Input

//let (dungeonSize, leafSize, roomSize) = 5, 5, 3
let (dungeonSize, leafSize, roomSize) = 50, 8, 6

let leftKeys = [Keys.Left;Keys.A]
let rightKeys = [Keys.Right;Keys.D]
let walkKeys = [Keys.Left;Keys.Right;Keys.Up;Keys.Down;Keys.A;Keys.D;Keys.W;Keys.S]

let handleCharacterState runState state facing =
    let elapsed = runState.elapsed
    let animFinished start = elapsed - start >= 10. * frameSpeed
    let newState = 
        match state with
        | _ when wasJustPressed Keys.R runState ->
            Standing elapsed
        | Dead -> 
            Dead
        | Dying start when animFinished start ->
            Dead
        | _ when wasJustPressed Keys.X runState -> 
            Dying elapsed
        | Standing _ when wasJustPressed Keys.C runState -> 
            Gesturing elapsed
        | Gesturing start when animFinished start ->
            Standing elapsed
        | Standing _ when isPressed Keys.F runState -> 
            Striking elapsed
        | Striking start when animFinished start ->
            Standing elapsed
        | Standing _ when isAnyPressed walkKeys runState -> 
            Walking elapsed
        | Walking _ when isAnyPressed walkKeys runState |> not -> 
            Standing elapsed
        | other -> other
    let newFacing = 
        match state with
        | Standing _ | Walking _ ->
            if isAnyPressed leftKeys runState then Left
            else if isAnyPressed rightKeys runState then Right
            else facing
        | _ -> facing
    CharacterRender (newState, newFacing) |> Some
    

let advanceGame runState worldState =
    match worldState with
    | _ when wasJustPressed Keys.Escape runState -> None
    | Some (MapView _) when wasJustPressed Keys.R runState ->
        dungeon dungeonSize leafSize roomSize |> MapView |> Some
    | None -> 
        dungeon dungeonSize leafSize roomSize |> MapView |> Some
    //| None -> CharacterRender (Standing 0., Left) |> Some
    | Some (CharacterRender (state, facing)) -> handleCharacterState runState state facing
    | other -> other