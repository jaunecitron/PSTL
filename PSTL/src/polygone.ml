open Point
open Environment
open Program
open Segment

type polygone = point list
		      
let add p polygone = p::polygone
			  
let height polygone =
  let y_min = ref (-1.) in
  let y_max = ref (-1.) in
  let rec aux p =
    match p with
      [] -> !y_max -. !y_min
     |h::t-> (if (h.y < !y_min) || (!y_min = -1.) then y_min := h.y);
	     (if (h.y > !y_max) || (!y_max = -1.) then y_max := h.y);
	     aux t
  in
  aux polygone		
			  
let width polygone =
  let x_min = ref (-1.) in
  let x_max = ref (-1.) in
  let rec aux p =
    match p with
      [] -> !x_max -. !x_min
     |h::t-> (if (h.x < !x_min) || (!x_min = -1.) then x_min := h.x);
	     (if (h.x > !x_max) || (!x_max = -1.) then x_max := h.x);
	     aux t
  in
  aux polygone

let barycentre polygone =
  let x_sum = ref 0. in
  let y_sum = ref 0. in
  let rec aux p n=
    match p with
      [] -> {x= !x_sum /. (float_of_int n); y= !y_sum /. (float_of_int n)}
     |h::t-> x_sum:= !x_sum +. h.x;
	     y_sum:= !y_sum +. h.y;
	     aux t (n+1) in
  aux polygone 0
  

let resize polygone w h =
  let point_barycentre = barycentre polygone in
  let width_pourcentage = w/.(width polygone) in
  let height_pourcentage = h/.(height polygone) in
  let rec aux p res =
    match p with
      [] -> res
     |h::t-> aux t ({x=h.x -. (point_barycentre.x -. h.x) *. width_pourcentage;
		   y=h.y -. (point_barycentre.y -. h.y) *. height_pourcentage} ::res) in
  aux polygone []
  
let polygone_to_segments polygone =
  match polygone with
    [] -> []
  | e::[] -> []
  | h::t -> let previous_elt = ref h in
	    let rec aux p res =
	      match p with
		[] -> res
	      | h::t -> let seg = create_segment !previous_elt h in
			previous_elt := h;
			aux t (seg::res) in
	    aux t []

	    
let print_polygone_current_place environment polygone =
  let reverse_polygone = List.rev polygone in
  match reverse_polygone with
    [] -> []
   |h::t-> let x = h.x in
	   let y = h.y in
	   let init_x = environment.pos.x in
	   let init_y = environment.pos.y in
	   let rec to_instructions p res =
	     match p with
	       [] -> (print_to environment init_x init_y) :: res
	      |h::t-> let instr = print_to environment (init_x+.h.x-.x) (init_y+.h.y-.y) in
		      to_instructions t (instr::res) in
	   to_instructions t []

let print_polygone environment polygone =
   match polygone with
    [] -> []
   |h::t-> let x = h.x in
	   let y = h.y in
	   let rec to_instructions p res =
	     match p with
	       [] -> (print_to environment x y) :: res
	      |h::t-> let instr = print_to environment h.x h.y in
		      to_instructions t (instr::res) in
	   to_instructions t [move_to environment x y]

