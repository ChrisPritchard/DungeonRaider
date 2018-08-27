module View

open GameCore
open Model
open Microsoft.Xna.Framework

let screenWidth, screenHeight = 800, 800
let resolution = Windowed (screenWidth, screenHeight)

let assetsToLoad = [
    Font ("default", "Content/coders_crux")
    Texture ("white", "./Content/white.png")
    TextureMap ("cleric", "./Content/Sprites/cleric.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("ranger", "./Content/Sprites/ranger.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("rogue", "./Content/Sprites/rogue.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("warrior", "./Content/Sprites/warrior.png", "./Content/Sprites/standard-key.csv")
    TextureMap ("wizard", "./Content/Sprites/wizard.png", "./Content/Sprites/standard-key.csv")
]

let (tx, ty) = 10, 10

let colourFor =
    function
    | Room -> Color.White
    | Corridor -> new Color(200,200,200)
    | Door -> Color.Brown
    | Wall -> new Color(50,50,50)

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

let getView (runState : RunState) worldState =
    let elapsed = runState.elapsed
    match worldState with
    | MapView map ->
        let blocks = map |> List.map (fun (Tile (x, y, kind)) -> 
            Image ("white", (x*tx,y*ty,tx,ty), colourFor kind))
        blocks
    | CharacterRender (state, facing) ->
        [
            MappedImage ("cleric", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64 - 256, 128, 128), Color.White)
            MappedImage ("ranger", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64 - 128, 128, 128), Color.White)
            MappedImage ("rogue", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64, 128, 128), Color.White)
            MappedImage ("warrior", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64 + 128, 128, 128), Color.White)
            MappedImage ("wizard", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64 + 256, 128, 128), Color.White)
        ]