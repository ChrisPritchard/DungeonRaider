module PathFinding

open AStar
open Util
open Model

let private neighbourDeltas = 
    [-1..1] |> Seq.collect (fun dx ->
    [-1..1] |> Seq.filter (fun dy -> dy <> dx || dy <> 0) 
    |> Seq.map (fun ny -> dx, ny))
    |> Seq.toList


let private notExists f s = Seq.exists f s |> not

let private astarConfig isOpen entities goal limit : AStar.Config<int * int> =
    let isClear x y = 
        isOpen x y 
        && (goal = (x, y)
        || entities 
            |> notExists (fun m -> 
                m.position = (x, y) 
                || match m.state with Walking (_, next::_) -> next = (x, y) | _ -> false))
    let neighbours (x, y) =
        neighbourDeltas 
        |> Seq.filter(fun (dx, dy) ->
            if abs dx + abs dy = 2 then
                isClear x (dy + y) && isClear (dx + x) y
            else true)
        |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
        |> Seq.filter (fun (nx, ny) -> isClear nx ny)
    let gScore (x1, y1) (x2, y2) = 
        if (abs (x2 - x1) + abs (y2 - y1)) = 2 then 1.4 else 1.
    let fScore = distanceBetween
    { neighbours = neighbours; gCost = gScore; fCost = fScore; maxIterations = limit }

let findPath start goal otherEntities isOpen limit =
    AStar.search start goal <| astarConfig isOpen otherEntities goal limit
    |> Option.bind (Seq.rev >> Seq.skip 1 >> Seq.toList >> Some)