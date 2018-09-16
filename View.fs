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

let originx, originy = midx, midy + playerheight*2/3

let worldPos (tx, ty) = tx * tilewidth, ty * tileheight

let currentWorldPos runState entity = 
    let wx, wy = entity.position |> worldPos
    match entity.path with
    | nextPos::_ when entity.moveStart <> 0. ->
        let distance = (runState.elapsed - entity.moveStart) / timeBetweenTiles
        let nx, ny = nextPos |> worldPos
        let dx, dy = nx - wx, ny - wy
        wx + int (float dx * distance), wy + int (float dy * distance)
    | _ -> wx, wy

let relativeTo entity runState (wx, wy) =
    let rx, ry = entity |> currentWorldPos runState
    let diffx, diffy = rx - wx, ry - wy
    originx - diffx, originy - diffy

let isVisible (x, y, width, height) =
    x + width/2 > 0 
    && x - width/2 < screenWidth 
    && y > 0 
    && y - height < screenHeight

let renderRect (wx, wy) (width, height) = 
    if showGrid then
        wx - width / 2 + 1, wy - height + 1, width - 2, height - 2
    else
        wx - width / 2, wy - height, width, height

let playerRenderRect = midx - playerwidth/2, midy - playerheight/2, playerwidth, playerheight

let tiles player runState map = 
    map 
    |> List.mapi (fun i (Tile (x, y, kind, adjacency)) -> 
        let rx, ry = (x, y) |> worldPos |> relativeTo player runState
        i, kind, adjacency, rx, ry)
    |> List.filter (fun (_, _, _, rx, ry) -> 
        isVisible (rx, ry, tilewidth, tileheight * 2))
    |> List.map (fun (i, kind, adjacency, rx, ry) -> 
        let normalHeight = renderRect (rx, ry) (tilewidth, tileheight)
        let doubleHeight = renderRect (rx, ry) (tilewidth, tileheight * 2)
        match kind with
        | Block -> 
            match wallFor adjacency i with
            | Some wall -> 
                MappedImage ("dungeon", wall, doubleHeight, Color.White)
            | _ ->
                MappedImage ("dungeon", sprintf "ceiling_%s" (keyForAdjacency adjacency Block i), normalHeight, Color.White)
        | StairsUp ->
            MappedImage ("dungeon", "wall_stairsup", doubleHeight, Color.White)
        | StairsDown index ->
            MappedImage ("dungeon", sprintf "stairsdown_%i" (index + 1), normalHeight, Color.White)
        | other -> 
            MappedImage ("dungeon", sprintf "floor_%s" (keyForAdjacency adjacency other i), normalHeight, Color.White))

let frameFor entity runState = 
    let frameFor start = (((runState.elapsed - start) % (10. * frameSpeed)) / frameSpeed) + 1. |> floor |> int
    let facing = match entity.facing with Left -> "left" | _ -> "right"
    match entity.state with
    | Standing start -> sprintf "stand%s%i" facing <| frameFor start
    | Gesturing start -> sprintf "gesture%s%i" facing <| frameFor start
    | Walking start -> sprintf "walk%s%i" facing <| frameFor start
    | Striking start -> sprintf "strike%s%i" facing <| frameFor start
    | Dying start -> sprintf "die%s%i" facing <| frameFor start
    | Dead -> sprintf "die%s10" facing

let getView runState worldState =
    match worldState with
    | Playing (map, player, monsters) ->
        [
            yield! tiles player runState map

            yield!
                [
                    yield! monsters |> List.map (fun monster -> 
                        let monsterPos = currentWorldPos runState monster
                        let mx, my = relativeTo player runState monsterPos
                        let rect = renderRect (mx, my - (tileheight/4)) monstersize
                        let monsterFrame = frameFor monster runState
                        monster.position, MappedImage ("minotaur", monsterFrame, rect, Color.White))
                    
                    let playerFrame = sprintf "%s_A" <| frameFor player runState
                    yield player.position, MappedImage ("rogue", playerFrame, playerRenderRect, Color.White)
                ] |> Seq.sortBy (fun ((x, y), _) -> y, x) |> Seq.map (fun (_, image) -> image)
            
            let mx, my = runState.mouse.position
            yield Image ("pointer", (mx, my, 20, 20), Color.White)

            if showPlayerPos then
                yield Text ("default", sprintf "%i, %i" (mx - midx) (my - midy), (20, 20), TopLeft, 0.5, Color.White)
        ]