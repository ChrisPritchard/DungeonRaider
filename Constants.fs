module Constants

open Microsoft.Xna.Framework.Input

let screenWidth, screenHeight = 1000, 1000
let quitKey = Keys.Escape

let showFPS = true // NOTE: disables the ability to debug
let showGrid = false
let showPlayerPos = false

let midx, midy = screenWidth / 2, screenHeight / 2

let dungeonSize, leafSize, roomSize, minCorridorLength = 60, 7, 5, 3

let tilewidth, tileheight = 48, 48
let playerwidth, playerheight = tilewidth * 3/2, tileheight * 3/2

let frameSpeed = 75.
let animationTime = frameSpeed * 10.
let hitTime = frameSpeed * 3.

let monsterSightRange = 4.5
let lightRadius = 10. * float tilewidth