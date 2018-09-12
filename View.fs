module View

open GameCore
open Constants
open Model
open Microsoft.Xna.Framework

let resolution = Windowed (screenWidth, screenHeight)

let assetsToLoad = [
    Font ("default", "Content/coders_crux")
    Texture ("white", "./Content/white.png")
    Texture ("pointer", "./Content/pointer.png")
    TextureMap ("dungeon", "./Content/Sprites/dungeon.png", "./Content/Sprites/dungeon-key.csv")
    TextureMap ("cleric", "./Content/Sprites/cleric.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("ranger", "./Content/Sprites/ranger.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("rogue", "./Content/Sprites/rogue.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("warrior", "./Content/Sprites/warrior.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("wizard", "./Content/Sprites/wizard.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("minotaur", "./Content/Sprites/minotaur.png", "./Content/Sprites/minotaur-key.csv")
]

let keyForAdjacency (adjacency : byte) kind index =
    let text = System.Convert.ToString(adjacency, 2).PadLeft(8, '0')
    match kind with
    | _ when List.contains adjacency [68uy;17uy] -> 
        sprintf "%s_%i" text (index % 2 + 1)
    | _ when List.contains adjacency [1uy;4uy;16uy;64uy] -> 
        sprintf "%s_%i" text (index % 3 + 1)
    | Block ->
        text
    | _ when adjacency = 0uy -> 
        sprintf "%s_%i" text (index % 9 + 1) 
    | _ when List.contains adjacency [193uy;112uy;28uy;7uy] -> 
        sprintf "%s_%i" text (index % 2 + 1)
    | _ ->
        text

let wallFor adjacency index =
    let has source check = source &&& check = check
    let hasAny source checks = checks |> List.exists (fun check -> source &&& check = check)
    if has adjacency 4uy |> not then
        None
    else
        let hasLeft = has adjacency 1uy
        let hasRight = has adjacency 16uy
        if hasLeft && hasRight then
            Some "wall_leftright"
        else if hasLeft then
            Some "wall_left"
        else if hasRight then
            Some "wall_right"
        else
            Some <| sprintf "wall_%i" (index % 4 + 1)

let relativeToPlayer (playerx, playery) (x, y) =
    let relx, rely = midx + (x - playerx) * tilewidth, midy + (y - playery) * tileheight
    relx - tilewidth / 2, rely - playerheight / 2

let tiles playerPosition map = 
    map 
    |> List.mapi (fun i (Tile (x, y, kind, adjacency)) -> 
        let rx, ry = relativeToPlayer playerPosition (x, y)
        i, kind, adjacency, rx, ry)
    |> List.filter (fun (_, _, _, rx, ry) -> 
        rx + tilewidth > 0 && rx < screenWidth && ry + tileheight > 0 && ry - tileheight < screenWidth)
    |> List.map (fun (i, kind, adjacency, rx, ry) -> 
        match kind with
        | Block -> 
            match wallFor adjacency i with
            | Some wall -> 
                MappedImage ("dungeon", wall, (rx, ry - tileheight, tilewidth, tileheight * 2), Color.White)
            | _ ->
                MappedImage ("dungeon", sprintf "ceiling_%s" (keyForAdjacency adjacency Block i), (rx, ry, tilewidth, tileheight), Color.White)
        | StairsUp ->
            MappedImage ("dungeon", "wall_stairsup", (rx, ry - tileheight, tilewidth, tileheight * 2), Color.White)
        | StairsDown index ->
            MappedImage ("dungeon", sprintf "stairsdown_%i" (index + 1), (rx, ry, tilewidth, tileheight), Color.White)
        | other -> 
            MappedImage ("dungeon", sprintf "floor_%s" (keyForAdjacency adjacency other i), (rx, ry, tilewidth, tileheight), Color.White))

let frameFor elapsed state facing = 
    let frameFor start = (((elapsed - start) % (10. * frameSpeed)) / frameSpeed) + 1. |> floor |> int
    let facing = match facing with Left -> "left" | _ -> "right"
    match state with
    | Standing start -> sprintf "stand%s%i" facing <| frameFor start
    | Gesturing start -> sprintf "gesture%s%i" facing <| frameFor start
    | Walking (_, start) -> sprintf "walk%s%i" facing <| frameFor start
    | Striking start -> sprintf "strike%s%i" facing <| frameFor start
    | Dying start -> sprintf "die%s%i" facing <| frameFor start
    | Dead -> sprintf "die%s10" facing

let playerRenderRect = midx - playerwidth/2, midy - playerheight/2, playerwidth, playerheight

let getView runState worldState =
    let elapsed = runState.elapsed
    match worldState with
    | Playing (map, player, monsters) ->
        [
            yield! tiles player.position map

            let playerx, playery = player.position
            yield!
                [
                    yield! monsters |> List.map (fun m -> 
                        let monsterx, monstery = m.position
                        let monsterFrame = frameFor elapsed m.state m.facing
                        let rx, ry = relativeToPlayer player.position (int monsterx - monsterwidth/2, int monstery - monsterheight)
                        let monsterRenderRect = rx, ry, monsterwidth, monsterheight
                        monsterx, monstery, MappedImage ("minotaur", monsterFrame, monsterRenderRect, Color.White))
                    
                    let playerFrame = sprintf "%s_A" <| frameFor elapsed player.state player.facing
                    yield playerx, playery, MappedImage ("rogue", playerFrame, playerRenderRect, Color.White)
                ] |> Seq.sortBy (fun (x, y, _) -> y, x) |> Seq.map (fun (_, _, image) -> image)
            
            let mx, my = runState.mouse.position
            yield Image ("pointer", (mx, my, 20, 20), Color.White)
        ]