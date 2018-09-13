module Constants

open Microsoft.Xna.Framework.Input

let screenWidth, screenHeight = 800, 800
let midx, midy = screenWidth / 2, screenHeight / 2

let dungeonSize, leafSize, roomSize, minCorridorLength = 30, 7, 5, 3

let tilewidth, tileheight = 48, 48
let playerwidth, playerheight = tilewidth * 3/2, tileheight * 3/2
let monsterwidth, monsterheight = playerwidth * 3/2, playerheight * 3/2

let frameSpeed = 75.
let timeBetweenTiles = 1000.

let walkSpeed = tilewidth / 16 |> float
// player jiggle
let boundaryx, boundaryyup, boundaryydown = float (playerwidth/4), float (playerheight/8), float (playerheight/16)

let quitKey = Keys.Escape

let leftKeys = [Keys.Left;Keys.A]
let rightKeys = [Keys.Right;Keys.D]
let upKeys = [Keys.Up;Keys.W]
let downKeys = [Keys.Down;Keys.S]

let deathKey = Keys.X
let gestureKey = Keys.C
let strikeKey = Keys.F
let resetKey = Keys.R