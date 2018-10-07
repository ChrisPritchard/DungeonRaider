
open GameCore.GameLoop

[<EntryPoint>]
let main _ =
    use game = 
        new GameLoop<Model.GameModel>(
            View.resolution, 
            View.assetsToLoad, 
            Controller.advanceGame, 
            View.getView, 
            Constants.showFPS)
    game.Run ()
    0