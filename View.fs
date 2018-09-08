module View

open GameCore
open Model
open Microsoft.Xna.Framework

let screenWidth, screenHeight = 800, 800
let midx, midy = screenWidth / 2, screenHeight / 2
let tx, ty = 32, 32
let pw, ph = tx * 3/2, ty * 3/2

let resolution = Windowed (screenWidth, screenHeight)

let assetsToLoad = [
    Font ("default", "Content/coders_crux")
    Texture ("white", "./Content/white.png")
    TextureMap ("dungeon", "./Content/Sprites/dungeon.png", "./Content/Sprites/dungeon-key.csv")
    TextureMap ("cleric", "./Content/Sprites/cleric.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("ranger", "./Content/Sprites/ranger.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("rogue", "./Content/Sprites/rogue.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("warrior", "./Content/Sprites/warrior.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("wizard", "./Content/Sprites/wizard.png", "./Content/Sprites/standard-key.csv")
]

let frameSpeed = 75.

let frameFor elapsed state facing = 
    let frameFor start = (((elapsed - start) % (10. * frameSpeed)) / frameSpeed) + 1. |> floor |> int
    let facing = match facing with Left -> "left" | _ -> "right"
    match state with
    | Standing start -> sprintf "stand%s%i_A" facing <| frameFor start
    | Gesturing start -> sprintf "gesture%s%i_A" facing <| frameFor start
    | Walking start -> sprintf "walk%s%i_A" facing <| frameFor start
    | Striking start -> sprintf "strike%s%i_A" facing <| frameFor start
    | Dying start -> sprintf "die%s%i_A" facing <| frameFor start
    | Dead -> sprintf "die%s10_A" facing

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

//  128 64  32
//  1       16
//  2   4   8

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

let getView runState worldState =
    let elapsed = runState.elapsed
    match worldState with
    | Playing (map, state, facing, (playerx, playery)) ->
        let blocks = 
            map 
            |> List.mapi (fun i (Tile (x, y, kind, adjacency)) -> 
                let rx, ry = midx + (x*tx) - playerx - tx/2, midy + (y*ty) - playery
                i, kind, adjacency, rx, ry)
            |> List.filter (fun (_, _, _, rx, ry) -> 
                rx + tx > 0 && rx < screenWidth && ry + ty > 0 && ry - ty < screenWidth)
            |> List.map (fun (i, kind, adjacency, rx, ry) -> 
                match kind with
                | Block -> 
                    match wallFor adjacency i with
                    | Some wall -> 
                        MappedImage ("dungeon", wall, (rx, ry - ty, tx, ty * 2), Color.White)
                    | _ ->
                        MappedImage ("dungeon", sprintf "ceiling_%s" (keyForAdjacency adjacency Block i), (rx, ry, tx, ty), Color.White)
                | other -> 
                    MappedImage ("dungeon", sprintf "floor_%s" (keyForAdjacency adjacency other i), (rx, ry, tx, ty), Color.White))
        [
            yield! blocks
            yield MappedImage ("rogue", frameFor elapsed state facing, (midx - pw/2, midy - ph/2, pw, ph), Color.White)
        ]