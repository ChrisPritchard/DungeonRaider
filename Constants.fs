module Constants

open Microsoft.Xna.Framework.Input

let screenWidth, screenHeight = 800, 800
let (dungeonSize, leafSize, roomSize, minCorridorLength) = 30, 7, 5, 3

let midx, midy = screenWidth / 2, screenHeight / 2
// tile size
let tx, ty = 48, 48
// player size
let pw, ph = tx * 3/2, ty * 3/2

let frameSpeed = 75.
let walkSpeed = tx / 16
// player jiggle
let boundaryx, boundaryy = pw/4, ph/8

let quitKey = Keys.Escape
let leftKeys = [Keys.Left;Keys.A]
let rightKeys = [Keys.Right;Keys.D]
let upKeys = [Keys.Up;Keys.W]
let downKeys = [Keys.Down;Keys.S]
let walkKeys = [Keys.Left;Keys.Right;Keys.Up;Keys.Down;Keys.A;Keys.D;Keys.W;Keys.S]
let deathKey = Keys.X
let gestureKey = Keys.C
let strikeKey = Keys.F
let resetKey = Keys.R