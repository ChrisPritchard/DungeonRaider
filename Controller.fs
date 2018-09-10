module Controller

open GameCore
open Model
open Bsp
open Constants

let nextState runState state =
    let elapsed = runState.elapsed
    let animFinished start = elapsed - start >= 10. * frameSpeed
    match state with
    | _ when wasJustPressed resetKey runState ->
        Standing elapsed
    | Dead -> 
        Dead
    | Dying start when animFinished start ->
        Dead
    | _ when wasJustPressed deathKey runState -> 
        Dying elapsed
    | Standing _ when wasJustPressed gestureKey runState -> 
        Gesturing elapsed
    | Gesturing start when animFinished start ->
        Standing elapsed
    | Standing _ when isPressed strikeKey runState -> 
        Striking elapsed
    | Walking _ when isPressed strikeKey runState -> 
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
    let checkBlock () =
        let blockx, blocky = x * tx, y * ty
        let isOnX =
            (playerx - boundaryx >= blockx && playerx - boundaryx < blockx + tx) // block is left
            || (playerx + boundaryx < blockx + tx && playerx + boundaryx >= blockx) // block is right
        let isOnY =
            (playery - boundaryyup >= blocky && playery - boundaryyup < blocky + ty) // block is above
            || (playery + boundaryydown < blocky + ty && playery + boundaryydown >= blocky) // block is below
        isOnX && isOnY
    match kind with
    | Block _ | StairsUp | StairsDown 1 -> 
        checkBlock ()
    | _ ->
        false

let getTileKind (x, y) =
    List.find (fun (Tile (ox, oy, _, _)) -> 
        ox * tx <= x && 
        ox * tx + tx > x && 
        oy * ty <= y && 
        oy * ty + ty > y)
    >> fun (Tile (_, _, kind, _)) -> kind

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
    | _ when wasJustPressed quitKey runState -> None
    | None -> 
        let map = dungeon dungeonSize leafSize roomSize minCorridorLength
        let position = map |> List.pick (fun (Tile (x, y, kind, _)) -> 
            match kind with StairsUp -> Some (x*tx + tx/2, (y+1)*ty + ty/2) | _ -> None)
        let (state, facing, position) = (Standing 0., Left, position)
        Playing (map, state, facing, position) |> Some
    | Some (Playing (map, state, facing, position)) -> 
        let newState = nextState runState state
        let newFacing = nextFacing runState newState facing
        let newPosition = nextPosition runState newState position map
        Playing (map, newState, newFacing, newPosition) |> Some
    | other -> other