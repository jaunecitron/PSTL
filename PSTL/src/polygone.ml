open Instruction
open Point
open Environment
open Program
open Droite
open Segment
open Printf

type polygone = point list
type box = {x_min:float;x_max:float;y_min:float;y_max:float}
	     
let create_box polygone =
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
  let box = create_box polygone in 
  let width_pourcentage = w/.(box.x_max -. box.x_min) in
  let height_pourcentage = h/.(box.y_max -. box.y_min) in
  let rec aux p res =
    match p with
      [] -> res
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

let compare direction p1 p2 =
  if direction == 0 then compare_ordonnee p1 p2 else compare_abscisse p1 p2
			   
(* direction correspond a la direction des droites remplissant le polygone
   direction vaut 0 si c'est horizontal, 1 si c'est vertical *)
let print_polygone_inner environment polygone direction shift_value =
  let segments = polygone_to_segments polygone in
  let polygone_box = create_box polygone in
  let intersections = ref [] in
  let rec aux pts res =
    match pts with
      [] -> res
     |e1::e2::t -> aux t ((print_to environment e2.x e2.y)::(move_to environment e1.x e1.y)::res)
     |h::t -> aux t ((print_to environment h.x h.y)::(move_to environment h.x h.y)::res) in
  let droite = ref {a=0.;b=0.;c=0.} in
  let taille_intervalle = if direction == 0 then (polygone_box.y_max-.polygone_box.y_min) else (polygone_box.x_max-.polygone_box.x_min) in
  let res = ref [] in
  let sens = ref 1 in
  let i = ref 0. in
  if direction == 0 then droite := {a=(-1.);b=0.;c=(-1.)*.polygone_box.y_min} else droite := {a=0.;b=1.;c=(-1.)*.polygone_box.x_min};
  while !i < taille_intervalle
  do
    intersections := List.sort
		       (fun p1 p2-> !sens * (compare direction p1 p2))
		       (intersections_segments_droite segments !droite);
    res := List.append (aux !intersections []) !res;
    intersections := [];
    droite := {a=(!droite).a;b=(!droite).b;c=(!droite).c-.shift_value};
    sens := !sens * -1;
    i := !i +. shift_value
  done;
  !res 
  
  
  
