module Util

let distanceBetween (x1, y1) (x2, y2) =
    sqrt <| (float x2 - float x1)**2. + (float y2 - float y1)**2.