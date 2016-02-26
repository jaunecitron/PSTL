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
			  
let widh polygone =
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
  
   			  
let print_polygone environment polygone =
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

  
