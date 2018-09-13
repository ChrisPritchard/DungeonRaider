module Controller

open GameCore
open Model
open Bsp
open Constants

let getTile x y map = List.item (x * dungeonSize + y) map

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
    // | Walking _ when isPressed strikeKey runState -> 
    //     Striking elapsed
    | Striking start when animFinished start ->
        Standing elapsed
    // | Standing _ when isAnyPressed walkKeys runState -> 
    //     Walking elapsed
    // | Standing _ when isMousePressed (true, false) runState -> 
    //     Walking elapsed
    // | Walking _ when (isAnyPressed walkKeys runState || isMousePressed (true, false) runState) |> not -> 
    //     Standing elapsed
    | other -> other

let nextFacing runState characterState facing = 
    let mx, _ = runState.mouse.position
    let mouseMove = isMousePressed (true, false) runState
    match characterState with
    | Standing _ | Walking _ ->
        if isAnyPressed leftKeys runState || (mouseMove && mx < midx) then Left
        else if isAnyPressed rightKeys runState || (mouseMove && mx > midx) then Right
        else facing
    | _ -> facing

let isBlocked (playerx, playery) (Tile (x, y, kind, _)) =
    let checkBlock () =
        let blockx, blocky = x * tilewidth |> float, y * tileheight |> float
        let isOnX =
            (playerx - boundaryx >= blockx && playerx - boundaryx < blockx + float tilewidth) // block is left
            || (playerx + boundaryx < blockx + float tilewidth && playerx + boundaryx >= blockx) // block is right
        let isOnY =
            (playery - boundaryyup >= blocky && playery - boundaryyup < blocky + float tileheight) // block is above
            || (playery + boundaryydown < blocky + float tileheight && playery + boundaryydown >= blocky) // block is below
        isOnX && isOnY
    match kind with
    | Block _ | StairsUp | StairsDown 1 -> 
        checkBlock ()
    | _ ->
        false

let getMouseDir (mx, my) (x, y) =
    let mx, my = mx - midx, my - midy
    let angle = abs (float mx) / abs (float my) |> atan
    let dx, dy = sin angle * walkSpeed, cos angle * walkSpeed
    x + dx * (if mx < 0 then -1. else 1.), y + dy * (if my < 0 then -1. else 1.)

let getKeysDir runState = 
    [
        leftKeys, -1, 0
        rightKeys, 1, 0
        upKeys, 0, -1
        downKeys, 0, 1
    ] |> List.fold (fun (rx, ry) (keys, dx, dy) -> 
        if isAnyPressed keys runState then 
            (rx + dx, ry + dy) 
        else 
            (rx, ry)) 
        (0, 0)

// let nextPosition runState characterState (x, y) tiles =
//     let (nx, ny) = 
//         match characterState with
//         | Walking _ ->
//             if isMousePressed (true, false) runState then 
//                 getMouseDir runState.mouse.position (x, y)
//             else
//                 getKeysDir runState (x, y)
//         | _ -> (x, y)    
    
//     let (px, py) = (int x / tilewidth, int y / tileheight)
//     let adjacentTiles = [-1..1] |> List.collect (fun ox -> [-1..1] |> List.map (fun oy -> getTile (ox + px) (oy + py) tiles))

//     let (bx, by) = adjacentTiles |> List.fold (fun (rx, ry) tile -> 
//         let (bx, by) = isBlocked (nx, y) tile, isBlocked (x, ny) tile
//         rx || bx, ry || by) (false, false)
//     (if bx then x else nx), (if by then y else ny)

let isOpen (x, y) map =
    let tile = List.tryFind (fun (Tile (tx, ty, _, _)) -> tx = x && ty = y) map
    match tile with
    | None -> false
    | Some (Tile (_, _, kind, _)) ->
        match kind with
        | Room | Door | Corridor -> true
        | _ -> false

let updatePlayerPath map runState player =
    let (x, y) = player.position
    match getKeysDir runState with
    | (0, 0) -> player
    | (dx, dy) when isOpen (x + dx, y + dy) map -> 
        { player with path = [(x + dx, y + dy)] }
    | _ -> player

let updateEntityFacing entity =
    let (x, _) = entity.position
    match entity.path with
    | (nx, _)::_ when nx < x -> 
        { entity with facing = Left }
    | (nx, _)::_ when nx > x -> 
        { entity with facing = Right }
    | _ -> entity

let updateEntityState entity =
    entity

let advancePlayer map runState =
    updatePlayerPath map runState
    >> updateEntityFacing 
    >> updateEntityState
    // calculate path update
    // calculate facing
    // calculate state

//     let newState = nextState runState player.state
//     let newFacing = nextFacing runState newState player.facing
// //    let newPosition = nextPosition runState newState player.position map
//     { player with state = newState; facing = newFacing }

let advanceMonster map runState monster = 
    monster

let newLevel () =
    let startPos = 
        List.pick (fun (Tile (x, y, kind, _)) -> 
            match kind with 
                | StairsUp -> 
                    Some (x, y + 1) 
                | _ -> None)

    let map = dungeon dungeonSize leafSize roomSize minCorridorLength
    let (px,py) = startPos map
    let player = { state = Standing 0.; facing = Left; position = (px, py); path = [] }
    let monster = { state = Standing 0.; facing = Left; position = (px + 2, py + 2); path = [] }
    Playing (map, player, [monster]) |> Some

let advanceGame runState worldState =
    match worldState with
    | _ when wasJustPressed quitKey runState -> None
    | None -> 
        newLevel ()
    | Some (Playing (map, player, monsters)) -> 
        let newPlayer = advancePlayer map runState player
        let newMonsters = monsters |> List.map (advanceMonster map runState)
        Playing (map, newPlayer, newMonsters) |> Some
    | other -> other