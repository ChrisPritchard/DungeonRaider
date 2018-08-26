module View

open GameCore
open Model
open Microsoft.Xna.Framework

let screenWidth, screenHeight = 800, 600
let resolution = Windowed (screenWidth, screenHeight)

let assetsToLoad = [
    Font ("default", "Content/coders_crux")
    Texture ("white", "./Content/white.png")
    TextureMap ("rogue", "./Content/Sprites/rogue.png", "./Content/Sprites/standard-key.csv")
]

let (tx, ty) = 10, 10

let colourFor =
    function
    | Room -> Color.White
    | Corridor -> new Color(200,200,200)
    | Door -> Color.Brown
    | Wall -> new Color(50,50,50)

let frameSpeed = 150.

let frameFor elapsed characterState = 
    let frameFor start = (((elapsed - start) % (10. * frameSpeed)) / frameSpeed) + 1. |> floor |> int
    match characterState with
    | Standing start -> sprintf "stand%i_A" <| frameFor start
    | Gesturing start -> sprintf "gesture%i_A" <| frameFor start
    | Walking start -> sprintf "walk%i_A" <| frameFor start
    | Striking start -> sprintf "strike%i_A" <| frameFor start
    | Dying start -> sprintf "die%i_A" <| frameFor start
    | Dead -> "die10_A"

let getView (runState : RunState) worldState =
    let elapsed = runState.elapsed
    match worldState with
    | MapView map ->
        let blocks = map |> List.map (fun (Tile (x, y, kind)) -> 
            Image ("white", (x*tx,y*ty,tx,ty), colourFor kind))
        blocks
    | CharacterRender state ->
        [
            MappedImage ("rogue", frameFor elapsed state, (screenWidth / 2 - 64, screenHeight / 2 - 64, 128, 128), Color.White)
        ]