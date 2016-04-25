open Instruction
open Point
open Environment
open Program
open Droite
open Segment
open Box
open Forme
open Printf

type polygone = point list
	     
let create_box_polygone polygone =
  let x_min = ref (-1.) in
  let x_max = ref (-1.) in
  let y_min = ref (-1.) in
  let y_max = ref (-1.) in
  let rec aux p =
    match p with
      [] -> {x_min= !x_min ; x_max= !x_max ; y_min = !y_min ; y_max = !y_max}
     |h::t-> (if (h.x < !x_min) || (!x_min = -1.) then x_min := h.x);
	     (if (h.x > !x_max) || (!x_max = -1.) then x_max := h.x);
	     (if (h.y < !y_min) || (!y_min = -1.) then y_min := h.y);
	     (if (h.y > !y_max) || (!y_max = -1.) then y_max := h.y);
	     aux t
  in
  aux polygone

let pi = 4. *. atan 1.
      
let polygone_regulier_angle point radius nb_cotes angle =
  let rec aux n res =
    if n = 0 then res
    else aux (n-1)
	     ({x= point.x +. (radius *. cos((2.*.(float_of_int (n-1))*.pi/.(float_of_int nb_cotes))+.angle));
	       y= point.y +. (radius *. sin((2.*.(float_of_int (n-1))*.pi/.(float_of_int nb_cotes))+.angle))}
	      :: res) in
  aux nb_cotes []
      
let polygone_regulier point radius nb_cotes =
  polygone_regulier_angle point radius nb_cotes pi
	     
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

let barycentre_polygone polygone =
  let x_sum = ref 0. in
  let y_sum = ref 0. in
  let rec aux p n=
    match p with
      [] -> {x= !x_sum /. (float_of_int n); y= !y_sum /. (float_of_int n)}
     |h::t-> x_sum:= !x_sum +. h.x;
	     y_sum:= !y_sum +. h.y;
	     aux t (n+1) in
  aux polygone 0
  

let resize_polygone polygone w h =
  let box = create_box_polygone polygone in
  let point_barycentre = barycentre_polygone polygone in
  let width_pourcentage = w/.(box.x_max -. box.x_min) in
  let height_pourcentage = h/.(box.y_max -. box.y_min) in
  let rec aux p res =
    match p with
      [] -> List.rev res
     |h::t-> aux t ({x=h.x -. (point_barycentre.x -. h.x) *. width_pourcentage;
		   y=h.y -. (point_barycentre.y -. h.y) *. height_pourcentage} ::res) in
  aux polygone []
  
let polygone_to_segments polygone =
  match polygone with
    [] -> []
  | h::t -> let init_elt = h in
	    let previous_elt = ref h in
	    let rec aux p res =
	      match p with
		[] -> (create_segment !previous_elt init_elt):: res
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
  let reverse_polygone = List.rev polygone in
   match reverse_polygone with
    [] -> []
   |h::t-> let x = h.x in
	   let y = h.y in
	   let rec to_instructions p res =
	     match p with
	       [] -> (print_to environment x y) :: res
	      |h::t-> let instr = print_to environment h.x h.y in
		      to_instructions t (instr::res) in
	   to_instructions t [move_to environment x y]
			   
let print_polygone_inner environment polygone direction shift_value =
  print_forme_inner environment (polygone_to_segments polygone) direction shift_value
