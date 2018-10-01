module Bsp

open Model

let rec createTree (random : System.Random) (minwidth, minheight) (x, y, width, height) = 
    let recGetFor x y width height = createTree random (minwidth, minheight) (x, y, width, height)

    let splitOnX () = 
        let wiggleRoom = width - (2*minwidth)
        let mid = random.Next(minwidth, minwidth + wiggleRoom + 1)
        Partition (Vertical, 
            recGetFor x y mid height, 
            recGetFor (x + mid) y (width - mid) height)
            
    let splitOnY () =
        let wiggleRoom = height - (2*minheight)
        let mid = random.Next(minheight, minheight + wiggleRoom + 1)
        Partition (Horizontal, 
            recGetFor x y width mid, 
            recGetFor x (y + mid) width (height - mid))

    if width <= minwidth*2 && height <= minheight*2 then 
        Leaf <| (x, y, width, height)
    else if width <= minwidth*2 then
        splitOnY ()
    else if height <= minheight*2 then
        splitOnX ()
    else
        match random.NextDouble() with
        | n when n >= 0.5 ->
            splitOnX ()
        | _ ->
            splitOnY ()