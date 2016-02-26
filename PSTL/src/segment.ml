open Droite
open Point

type segment = {mutable d:droite; p:point; q:point}

let create_segment p1 p2 =
  {d=create_droite p1 p2; p=p1; q=p2}
		 
		 (*
let create_segment p1 p2 =
  let d_res = create_droite p1 p2 in
  let x_min_res = ref 0. in
  let x_max_res = ref 0. in
  let y_min_res = ref 0. in
  let y_max_res = ref 0. in
  if p1.x < p2.x then
    begin
      x_min_res := p1.x;
      x_max_res := p2.x
    end
  else
    begin
      x_min_res := p2.x;
      x_max_res := p1.x
    end
  ;
  if p1.y < p2.y then
    begin
      y_min_res := p1.y;
      y_max_res := p2.y
    end
  else
    begin
      y_min_res := p2.y;
      y_max_res := p1.y
    end
  ;
    {d=d_res; p=p1; q=p2; x_min=!x_min_res; x_max=!x_max_res; y_min=!y_min_res; y_max=!y_max_res}
		  *)

let intersection_segment_droite seg droite res=
  try
    (intersection seg.d droite) :: res
  with
    NotFound -> res
  | InfiniteResult -> seg.p :: res
