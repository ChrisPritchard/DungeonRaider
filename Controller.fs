module Controller

open GameCore
open Model
open Bsp
open Constants

let getTile x y map = 
    if x < 0 || y < 0 || x >= dungeonSize || y >= dungeonSize then None
    else Some <| List.item (x * dungeonSize + y) map

let mouseTile x y runState = 
    let mx, my = runState.mouse.position
    let relx, rely = midx - mx, midy - my

    let tilex, tiley = 
        (if relx < 0 then relx - tilewidth/2 else relx + tilewidth/2) / tilewidth, 
        if rely < 0 then float rely / float tileheight |> floor |> int else rely / tileheight
    x - tilex, y - tiley - 1

let isOpen x y map =
    match getTile x y map with
    | None -> false
    | Some (Tile (_, _, kind, _)) ->
        match kind with
        | Room | Door | Corridor -> true
        | _ -> false

let neighbourDeltas = 
    [-1..1] |> Seq.collect (fun dx ->
    [-1..1] |> Seq.filter (fun dy -> dy <> dx || dy <> 0) 
    |> Seq.map (fun ny -> dx, ny))
    |> Seq.toList

let astarConfig map entities : AStar.Config<int * int> =
    let isClear x y = 
        isOpen x y map 
        && entities |> Seq.forall (fun m -> 
            m.position <> (x, y)
            && match m.path with next::_ -> next <> (x, y) | _ -> true) 
    let neighbours (x, y) =
        neighbourDeltas 
        |> Seq.filter(fun (dx, dy) ->
            if abs dx + abs dy = 2 then
                isClear x (dy + y) && isClear (dx + x) y
            else true)
        |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
        |> Seq.filter (fun (nx, ny) -> isClear nx ny)
    let gScore (x1, y1) (x2, y2) = 
        if (abs (x2 - x1) + abs (y2 - y1)) = 2 then 1.4 else 1.
    let fScore (x, y) (gx, gy) = 
        sqrt ((float gx - float x)**2. + (float gy - float y)**2.)
    { neighbours = neighbours; gCost = gScore; fCost = fScore }
   
let getNewPlayerPath map monsters runState (x, y) =
    if isMousePressed (true, false) runState then
        let mx, my = mouseTile x y runState
        if isOpen mx my map then 
            AStar.search (x, y) (mx, my) (astarConfig map monsters) |> Option.bind (Seq.rev >> Seq.toList >> Some)
        else
            None
    else
        None

let updateEntityPosition runState pathFinder entity =
    match entity.path with
    | next::rest when runState.elapsed - entity.moveStart >= timeBetweenTiles ->
        let newPath = 
            match pathFinder runState next with
            | Some (_::path) -> path 
            | _ -> rest
        { entity with position = next; path = newPath; moveStart = runState.elapsed }
    | [] -> 
        match pathFinder runState entity.position with
        | Some (_::path) -> { entity with path = path; moveStart = runState.elapsed } 
        | _ -> entity
    | _ -> entity

let updateEntityFacing entity =
    let (x, _) = entity.position
    match entity.path with
    | (nx, _)::_ when nx < x -> 
        { entity with facing = Left }
    | (nx, _)::_ when nx > x -> 
        { entity with facing = Right }
    | _ -> entity

let updateEntityState entity =
    match entity.path with
    | _::_ when entity.moveStart <> 0. -> 
        match entity.state with
        | Walking _ -> entity
        | _ -> { entity with state = Walking entity.moveStart }
    | _ ->
        match entity.state with
        | Standing _ -> entity
        | _ -> { entity with state = Standing entity.moveStart }

let advancePlayer map monsters runState =
    updateEntityPosition runState (getNewPlayerPath map monsters)
    >> updateEntityFacing 
    >> updateEntityState

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
    let px, py = startPos map
    let player = { state = Standing 0.; facing = Left; position = (px, py); path = []; moveStart = 0. }
    let monster = { state = Standing 0.; facing = Left; position = (px + 2, py + 2); path = []; moveStart = 0. }
    Playing (map, player, [monster]) |> Some

let advanceGame runState worldState =
    match worldState with
    | _ when wasJustPressed quitKey runState -> None
    | None -> 
        newLevel ()
    | Some (Playing (map, player, monsters)) -> 
        let newPlayer = advancePlayer map monsters runState player
        let newMonsters = monsters |> List.map (advanceMonster map runState)
        Playing (map, newPlayer, newMonsters) |> Some
    | other -> other