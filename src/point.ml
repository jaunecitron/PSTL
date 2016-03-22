open Printf

type point = {mutable x:float;mutable y:float}

let epsilon = 1.0e-10
let (=.) a b = (abs_float (a-.b)) < epsilon
	       
let distance p1 p2 = sqrt((p2.y -. p1.y)*.(p2.y -. p1.y) +.
		       (p2.x -. p1.x)*.(p2.x -. p1.x))

let distance_carree p1 p2 = (p2.y -. p1.y)*.(p2.y -. p1.y) +.
		       (p2.x -. p1.x)*.(p2.x -. p1.x)
			 
let point_to_string p = sprintf "(%f;%f)" p.x p.y

let compare_abscisse p1 p2 =
  int_of_float (p1.x -. p2.x)

let compare_ordonnee p1 p2 =
  int_of_float (p1.y -. p2.y)

let compare_to_point origin p1 p2 =
  int_of_float ((distance_carree p1 origin) -. (distance_carree p2 origin))
