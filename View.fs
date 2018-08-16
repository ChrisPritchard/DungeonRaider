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

let (tx, ty) = 40, 40

let getView runState worldState =
    match worldState with
    | MapView map ->
        let blocks = map |> List.map (fun (Tile (x, y, wall)) -> 
            Image ("white", (x*tx,y*ty,tx,ty), if wall then Color.Gray else Color.White))
        blocks