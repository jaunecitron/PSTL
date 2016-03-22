open Droite
open Point
open Printf
       
type segment = {mutable droite:droite; p:point; q:point}

let create_segment p1 p2 =
  {droite=(create_droite p1 p2); p=p1; q=p2}

let segment_to_string segment =
  sprintf "Segment A%s B%s d'équation : %s" (point_to_string segment.p) (point_to_string segment.q) (droite_to_string segment.droite)
	  
let intersection_segment_droite seg droite=
  try
    let p = intersection seg.droite droite in
    let x_min = min seg.p.x seg.q.x in
    let x_max = max seg.p.x seg.q.x in
    let y_min = min seg.p.y seg.q.y in
    let y_max = max seg.p.y seg.q.y in
    if ((x_min < p.x) && (x_max > p.x) || ((=.) p.x x_min) || ((=.) p.x x_max)) && ((y_min < p.y) && (y_max > p.y) || ((=.) p.y y_min) || ((=.) p.y y_max)) then
      [p]
    else
      []
  with
    NotFound -> []
  | InfiniteResult -> [seg.p;seg.q]

let intersections_segments_droite segments droite=
  let rec aux l res=
    match l with
      [] -> res
     |h::t-> aux t (List.append (intersection_segment_droite h droite) res) in
  aux segments []
