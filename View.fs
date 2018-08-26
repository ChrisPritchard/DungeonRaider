module View

open GameCore
open Model
open Microsoft.Xna.Framework

let screenWidth, screenHeight = 800, 600
let resolution = Windowed (screenWidth, screenHeight)

let assetsToLoad = [
    Font ("default", "Content/coders_crux")
    Texture ("white", "./Content/white.png")
]

let (tx, ty) = 10, 10

let colourFor =
    function
    | Room -> Color.White
    | Corridor -> new Color(200,200,200)
    | Door -> Color.Brown
    | Wall -> new Color(50,50,50)

let getView runState worldState =
    match worldState with
    | MapView map ->
        let blocks = map |> List.map (fun (Tile (x, y, kind)) -> 
            Image ("white", (x*tx,y*ty,tx,ty), colourFor kind))
        blocks