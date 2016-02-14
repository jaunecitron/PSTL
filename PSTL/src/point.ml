open Printf

type point = {mutable x:float;mutable y:float}

let distance p1 p2 = sqrt((p2.y -. p1.y)*.(p2.y -. p1.y) +.
		       (p2.x -. p1.x)*.(p2.x -. p1.x))

let point_to_string p = sprintf "(%f;%f)" p.x p.y

