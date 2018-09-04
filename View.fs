module View

open GameCore
open Model
open Microsoft.Xna.Framework

let screenWidth, screenHeight = 800, 800
let midx, midy = screenWidth / 2, screenHeight / 2
let tx, ty = 96, 96

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

let frameSpeed = 150.

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

let wallFor adjacency index =
    if adjacency &&& 32uy <> 32uy then
        None
    else if adjacency &&& 128uy = 128uy && adjacency &&& 8uy = 8uy then
        None // TODO both ends
    else if adjacency &&& 128uy = 128uy then
        None // TODO left end
    else if adjacency &&& 8uy = 8uy then
        None // TODO right end
    else
        None // TODO mid wall

let getView runState worldState =
    let elapsed = runState.elapsed
    match worldState with
    | Playing (map, state, facing, (playerx, playery)) ->
        let blocks = 
            map 
            |> List.mapi (fun i (Tile (x, y, kind, adjacency)) -> 
                let (ox,oy,ow,oh) = (midx + (x*tx) - playerx - tx/2, midy + (y*ty) - playery, tx, ty)
                match kind with
                | Block -> 
                    match wallFor adjacency i with
                    | Some wall -> 
                        [
                            MappedImage ("dungeon", sprintf "ceiling_%s" (keyForAdjacency adjacency Block i), (ox,oy - ty*2,ow,oh), Color.White)
                            MappedImage ("dungeon", wall, (ox,oy - ty,ow,oh * 2), Color.White)
                        ]
                    | _ ->
                        [MappedImage ("dungeon", sprintf "ceiling_%s" (keyForAdjacency adjacency Block i), (ox,oy,ow,oh), Color.White)]
                | other -> 
                    [MappedImage ("dungeon", sprintf "floor_%s" (keyForAdjacency adjacency other i), (ox,oy,ow,oh), Color.White)])
        [
            yield! List.concat blocks
            yield MappedImage ("rogue", frameFor elapsed state facing, (midx - tx/2, midy - tx/2, tx, ty), Color.White)
        ]