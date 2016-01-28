open Point
open Environment
open Program

type polygone = point list
	    
let add p polygone = p::polygone
			  
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
