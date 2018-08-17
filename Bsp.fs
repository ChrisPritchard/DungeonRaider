module Bsp

open Model

let random = new System.Random()
let minDivide = 5
let minRoomSize = 3

let range = 
    let maxInt = System.Int32.MaxValue
    List.fold (fun (mix, miy, mox, moy) (x,y) -> 
        (min mix x, min miy y, max mox x, max moy y)) 
        (maxInt, maxInt, 0, 0)

let rec bspDivide minSize list = 
    let minRange = minSize * 2
    let minx, miny, maxx, maxy = range list
    let cantx, canty = maxx - minx < minRange, maxy - miny < minRange
    if cantx && canty then 
        [list]
    else
        let xdivide () =
            let mid = random.Next(minx + minSize, maxx - minSize)
            fun (x,_) -> x > mid
        let ydivide () =
            let mid = random.Next(miny + minSize, maxy - minSize)
            fun (_,y) -> y > mid
        let divider = 
            if cantx then ydivide ()
            else if canty then xdivide ()
            else match random.Next(2) with 0 -> xdivide () | _ -> ydivide ()
        let left, right = List.partition divider list
        List.concat [bspDivide minSize left; bspDivide minSize right]

let insertRoom list = 
    let (x, y, w, h) = range list
    let (mw, mh) = w - x, h - y
    let (rw, rh) =  random.Next(minRoomSize, mw - 1), 
                    random.Next(minRoomSize, mh - 1)
    list |> List.map (fun (ox, oy) -> Tile (ox, oy, not (ox <= x + rw && oy <= y + rh)))

let dungeon dim = 
    [0..dim-1] |> List.collect (fun x -> 
        [0..dim-1] |> List.map (fun y -> 
            x, y))
            |> bspDivide minDivide
            |> List.collect insertRoom