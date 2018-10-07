module Controller

open GameCore.GameModel
open Model
open Bsp
open Constants
open Util
open PathFinding

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

let advanceEntity runState enemies pathFinder entity =
    let elapsed = runState.elapsed
    match entity.state with
    | Walking (startTime, path) ->
        match path with
        | next::rest when elapsed - startTime > entity.timeBetweenTiles ->
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
        | next::_ ->
            match Seq.tryFind (fun m -> m.position = next) enemies with
            | Some enemy -> { entity with state = Striking (runState.elapsed, enemy, false) }
            | _ -> entity
        | _ -> entity
    | Standing _ ->
        match pathFinder runState entity.position with
        | Some path when path <> [] -> { entity with state = Walking (elapsed, path) } 
        | _ -> entity
    | Striking (startTime, enemy, false) when elapsed - startTime > animationTime/2. -> 
        { entity with state = Striking (startTime, enemy, true); events = [Struck enemy] }
    | Striking (startTime, _, _) when elapsed - startTime > animationTime -> 
        { entity with state = Standing elapsed }
    | Hit startTime when elapsed - startTime > hitTime ->
        { entity with state = Standing elapsed }
    | Dying startTime when elapsed - startTime > animationTime ->
        { entity with state = Dead }
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

let applyHits enemies runState target = 
    if target.health = 0 then target
    else
        let hits = 
            enemies 
                |> Seq.collect (fun m -> 
                    m.events |> Seq.filter (fun evt -> 
                        match evt with Struck enemy when enemy.position = target.position -> true | _ -> false))
                |> Seq.length
        if hits >= target.health then 
            { target with health = 0; state = Dying runState.elapsed }
        else if hits > 0 then 
            { target with health = target.health - hits; state = Hit runState.elapsed }
        else target


let getNewPlayerPath map monsters runState playerPosition =
    tryGetMouseTile playerPosition runState 
    |> Option.bind(fun (mx, my) -> 
        if isOpen map mx my then 
            findPath playerPosition (mx, my) monsters (isOpen map) (Some 20)
        else
            None)

let seekOutPlayer map monsters player monsterPosition =
    if player.health = 0 then None
    else
        let otherMonsters = monsters |> Seq.filter (fun m -> m.position <> monsterPosition)
        if distanceBetween monsterPosition player.position > monsterSightRange then None
        else
            findPath monsterPosition player.position otherMonsters (isOpen map) (Some 10)

let seekOutOrigin map monsters origin monsterPosition =
    let otherMonsters = monsters |> Seq.filter (fun m -> m.position <> monsterPosition)
    findPath monsterPosition origin otherMonsters (isOpen map) (Some 30)

let advancePlayer map monsters runState player =
    let pathFinder = fun runState position ->
        getNewPlayerPath map monsters runState position
    { player with events = [] } 
        |> advanceEntity runState monsters pathFinder
        |> updateEntityFacing 

let advanceMonster map monsters player runState monster = 
    let pathFinder = fun _ position ->
        seekOutPlayer map monsters player position
        |> Option.orElse (seekOutOrigin map monsters monster.origin position)
    { monster with events = [] }
        |> advanceEntity runState [player] pathFinder
        |> updateEntityFacing 

let advancePlaying runState map player monsters = 
    let nearEnemies = List.filter (fun m -> 
        m.health > 0 && isVisible <| renderRect (currentWorldPos runState m) m.size) monsters
    let newPlayer = advancePlayer map nearEnemies runState player
    let newMonsters = monsters |> List.map (fun m ->
        if isVisible <| renderRect (currentWorldPos runState m) m.size then
            m |> advanceMonster map nearEnemies newPlayer runState |> applyHits [newPlayer] runState
        else
            m)
    let finalPlayer = newPlayer |> applyHits newMonsters runState
    Playing (map, finalPlayer, newMonsters) |> Some

let newLevel () =
    let startPos = 
        List.pick (fun (Tile (x, y, kind, _)) -> 
            match kind with 
                | StairsUp -> 
                    Some (x, y + 1) 
                | _ -> None)

    let map = dungeon dungeonSize leafSize roomSize minCorridorLength
    let px, py = startPos map
    let player = newRogue (px, py)
    let monsters = [
        newMinotaur (px + 3, py + 4)
        newSkeleton (px + 4, py + 3)
    ]
    Playing (map, player, monsters) |> Some

let advanceGame runState worldState =
    match worldState with
    | _ when wasJustPressed quitKey runState -> None
    | None -> 
        newLevel ()
    | Some (Playing (map, player, monsters)) -> 
        advancePlaying runState map player monsters
    | other -> other