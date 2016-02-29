open Droite
open Point

type segment = {mutable d:droite; p:point; q:point}

let create_segment p1 p2 =
  {d=create_droite p1 p2; p=p1; q=p2}

let intersection_segment_droite seg droite res=
  try
    let p = intersection seg.d droite in
    let x_min = min seg.p.x seg.q.x in
    let x_max = max seg.p.x seg.q.x in
    let y_min = min seg.p.y seg.q.y in
    let y_max = max seg.p.y seg.q.y in
    if ( (x_min <= p.x) && (x_max >= p.x) && (y_min <= p.y) && (y_max >= p.y) ) then
      p :: res
    else
      res
  with
    NotFound -> res
  | InfiniteResult -> seg.p :: res
