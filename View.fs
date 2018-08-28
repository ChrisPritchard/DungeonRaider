module View

open GameCore
open Model
open Microsoft.Xna.Framework

let screenWidth, screenHeight = 800, 800
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

let stringToByte s = 
    s 
    |> Seq.rev 
    |> Seq.mapi (fun i c -> 
        c.ToString() 
        |> int 
        |> fun ci -> ci * pown 2 i) 
    |> Seq.sum
    |> byte

let validCeilings = 
    System.IO.File.ReadAllLines("./Content/Sprites/dungeon-key.csv")
    |> Seq.skip 1
    |> Seq.map (fun line -> 
        line.Split(',') 
        |> Seq.head 
        |> fun s -> s.Split('_') |> Seq.item 1 
        |> stringToByte)
    |> Seq.toList

let (tx, ty) = 160, 160

let colourFor =
    function
    | Room -> Color.White
    | Corridor -> new Color(200,200,200)
    | Door -> Color.Brown
    | Wall _ -> new Color(50,50,50)

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

let keyForAdjacency (adjacency : byte) =
    if List.contains adjacency validCeilings |> not then
        "11111111"
    else
        let text = System.Convert.ToString(adjacency, 2).PadLeft(8, '0')
        if List.contains adjacency [1uy;4uy;16uy;17uy;64uy;68uy] then text + "_1" else text

let getView (runState : RunState) worldState =
    let elapsed = runState.elapsed
    match worldState with
    | MapView map ->
        let blocks = map |> List.map (fun (Tile (x, y, kind)) -> 
            match kind with
            | Wall key -> MappedImage ("dungeon", sprintf "ceiling_%s" (keyForAdjacency key), (x*tx,y*ty,tx,ty), Color.White)
            | _ -> Image ("white", (x*tx,y*ty,tx,ty), colourFor kind))
        blocks
    | CharacterRender (state, facing) ->
        [
            MappedImage ("cleric", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64 - 256, 128, 128), Color.White)
            MappedImage ("ranger", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64 - 128, 128, 128), Color.White)
            MappedImage ("rogue", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64, 128, 128), Color.White)
            MappedImage ("warrior", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64 + 128, 128, 128), Color.White)
            MappedImage ("wizard", frameFor elapsed state facing, (screenWidth / 2 - 64, screenHeight / 2 - 64 + 256, 128, 128), Color.White)
        ]