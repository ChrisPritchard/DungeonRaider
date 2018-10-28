open GameCore.GameModel
open GameCore.GameRunner
open Microsoft.Xna.Framework

[<EntryPoint>]
let main _ =

    let config = {
        clearColour = Some Color.Black
        assetsToLoad = View.assetsToLoad
        resolution = View.resolution
        fpsFont = Some "default"   
    }

    runGame config Controller.advanceGame View.getView

    0