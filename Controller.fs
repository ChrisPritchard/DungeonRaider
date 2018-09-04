module Controller

open GameCore
open Model
open Bsp
open View
open Microsoft.Xna.Framework.Input

let (dungeonSize, leafSize, roomSize) = 40, 6, 4
let walkSpeed = tx / 16
let boundaryx, boundaryy = tx/4, ty/3

let leftKeys = [Keys.Left;Keys.A]
let rightKeys = [Keys.Right;Keys.D]
let upKeys = [Keys.Up;Keys.W]
let downKeys = [Keys.Down;Keys.S]
let walkKeys = [Keys.Left;Keys.Right;Keys.Up;Keys.Down;Keys.A;Keys.D;Keys.W;Keys.S]

let nextState runState state =
    let elapsed = runState.elapsed
    let animFinished start = elapsed - start >= 10. * frameSpeed
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

let nextFacing runState characterState facing = 
    match characterState with
    | Standing _ | Walking _ ->
        if isAnyPressed leftKeys runState then Left
        else if isAnyPressed rightKeys runState then Right
        else facing
    | _ -> facing

let isBlocked (playerx, playery) (Tile (x, y, kind, _)) =
    match kind with
    | Block _ -> 
        let blockx, blocky = x * tx, y * ty
        let isOnX = // right left
            (playerx + boundaryx >= blockx && playerx + boundaryx < blockx + tx) 
            || (playerx - boundaryx < blockx && playerx - boundaryx + tx > blockx)
        let isOnY = // below above
            (playery + boundaryy >= blocky && playery + boundaryy < blocky + ty) 
            || (playery - boundaryy < blocky && playery - boundaryy + ty > blocky)
        isOnX && isOnY
    | _ ->
        false

let nextPosition runState characterState (x, y) tiles =
    let (nx, ny) = 
        match characterState with
        | Walking _ ->
            [
                leftKeys, -walkSpeed, 0
                rightKeys, walkSpeed, 0
                upKeys, 0, -walkSpeed
                downKeys, 0, walkSpeed
            ] |> List.fold (fun (rx, ry) (keys, dx, dy) -> 
                if isAnyPressed keys runState then 
                    (rx + dx, ry + dy) 
                else 
                    (rx, ry)) (x, y)
        | _ -> (x, y)
    let (bx, by) = tiles |> List.fold (fun (rx, ry) tile -> 
        let (bx, by) = isBlocked (nx, y) tile, isBlocked (x, ny) tile
        rx || bx, ry || by) (false, false)
    (if bx then x else nx), (if by then y else ny)

let advanceGame runState worldState =
    match worldState with
    | _ when wasJustPressed Keys.Escape runState -> None
    | None -> 
        let map = dungeon dungeonSize leafSize roomSize
        let position = map |> List.pick (fun (Tile (x, y, kind, _)) -> 
            match kind with Block _ -> None | _ -> Some (x*tx, y*ty))
        let (state, facing, position) = (Standing 0., Left, position)
        Playing (map, state, facing, position) |> Some
    | Some (Playing (map, state, facing, position)) -> 
        let newState = nextState runState state
        let newFacing = nextFacing runState newState facing
        let newPosition = nextPosition runState newState position map
        Playing (map, newState, newFacing, newPosition) |> Some
    | other -> other