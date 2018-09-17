module Controller

open GameCore
open Model
open Bsp
open Constants

let getTile x y map = 
    if x < 0 || y < 0 || x >= dungeonSize || y >= dungeonSize then None
    else Some <| List.item (x * dungeonSize + y) map

let tryGetMouseTile playerPosition runState = 
    if not <| isMousePressed (true, false) runState then None
    else
        let mx, my = runState.mouse.position
        let relx, rely = midx - mx, midy - my
        let tilex, tiley = 
            (if relx < 0 then relx - tilewidth/2 else relx + tilewidth/2) / tilewidth, 
            if rely < 0 then float rely / float tileheight |> floor |> int else rely / tileheight
        let x, y = playerPosition
        Some <| (x - tilex, y - tiley - 1)

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

let notExists f s = Seq.exists f s |> not

let astarConfig map entities goal : AStar.Config<int * int> =
    let isClear x y = 
        isOpen x y map 
        && (goal = (x, y)
        || entities 
            |> notExists (fun m -> 
                m.position = (x, y) 
                || match m.state with Walking (_, next::_) -> next = (x, y) | _ -> false))
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
    tryGetMouseTile (x, y) runState 
    |> Option.bind(fun (mx, my) -> 
        if isOpen mx my map then 
            AStar.search (x, y) (mx, my) (astarConfig map monsters (mx, my)) 
            |> Option.bind (Seq.rev >> Seq.skip 1 >> Seq.toList >> Some)
        else
            None)

let updateEntityPosition runState monsters pathFinder entity =
    let elapsed = runState.elapsed
    match entity.state with
    | Walking (startTime, path) ->
        match path with
        | next::_ when Seq.exists (fun m -> m.position = next) monsters ->
            { entity with state = Standing runState.elapsed }
        | next::rest when elapsed - startTime > timeBetweenTiles ->
            let newPath = 
                match pathFinder runState next with
                | Some path -> path 
                | _ -> rest
            { entity with 
                position = next
                state = 
                    match newPath with 
                    | [] -> Standing elapsed 
                    | _ -> Walking (elapsed, newPath) }
        | _ -> entity
    | Standing _ ->
        match pathFinder runState entity.position with
        | Some path -> { entity with state = Walking (elapsed, path) } 
        | _ -> entity
    | _ -> entity

let updateEntityFacing entity =
    let (x, _) = entity.position
    match entity.state with
    | Walking (_, path) ->
        let (nx, _) = List.head path
        if nx < x then 
            { entity with facing = Left }
        else
            { entity with facing = Right }
    | _ -> entity

let updateEntityTarget runState enemies pathFinder entity =
    let elapsed = runState.elapsed
    match entity.state with
    | Striking startTime when elapsed - startTime > animationTime -> 
        { entity with state = Standing elapsed }
    | Standing _ ->
        let target = 
            pathFinder runState entity.position 
            |> Option.bind (fun path -> 
                let next = List.head path
                enemies |> Seq.tryFind (fun m -> m.position = next))
        match target with
        | Some _ -> { entity with state = Striking elapsed } // todo add enemy to attacking
        | _ -> entity
    | _ -> entity

let advancePlayer map monsters runState =
    let pathFinder = getNewPlayerPath map monsters
    updateEntityPosition runState monsters pathFinder
    >> updateEntityFacing 
    >> updateEntityTarget runState monsters pathFinder

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
    let player = { state = Standing 0.; facing = Left; position = (px, py) }
    let monster = { state = Standing 0.; facing = Left; position = (px + 2, py + 2) }
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