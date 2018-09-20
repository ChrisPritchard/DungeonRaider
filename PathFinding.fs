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

let castRay (x0, y0) (x1, y1) =
    let dx = x1 - x0
    let dy = y1 - y0

    let (result, _, _) = 
        [x0..(if x1 < x0 then -1 else 1)..x1] |> List.fold (fun (plotted, y, D) x -> 
            let next = (x, y)::plotted
            let (ny, nD) = 
                if D > 0 then y + 1, D - 2*dx else y, D + 2*dy
            next, ny, nD
        ) ([], y0, 2*dy - dx)

    result |> List.rev