module Constants

open Microsoft.Xna.Framework.Input

let screenWidth, screenHeight = 800, 800
let quitKey = Keys.Escape

let midx, midy = screenWidth / 2, screenHeight / 2

let dungeonSize, leafSize, roomSize, minCorridorLength = 30, 7, 5, 3

let tilewidth, tileheight = 48, 48
let playerwidth, playerheight = tilewidth * 3/2, tileheight * 3/2
let monsterwidth, monsterheight = playerwidth * 3/2, playerheight * 3/2

let frameSpeed = 75.
let timeBetweenTiles = 250.