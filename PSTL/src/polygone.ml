open Instruction
open Point
open Environment
open Program
open Droite
open Segment
open Printf

type polygone = point list
type box = {x_min:float;x_max:float;y_min:float;y_max:float}
	     
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

let create_box_forme forme =
  let x_min = ref (-1.) in
  let x_max = ref (-1.) in
  let y_min = ref (-1.) in
  let y_max = ref (-1.) in
  let rec aux f =
    match f with
      [] -> {x_min= !x_min ; x_max= !x_max ; y_min = !y_min ; y_max = !y_max}
     |h::t-> (if (h.p.x < !x_min) || (!x_min = -1.) then x_min := h.p.x);
	     (if (h.p.x > !x_max) || (!x_max = -1.) then x_max := h.p.x);
	     (if (h.p.y < !y_min) || (!y_min = -1.) then y_min := h.p.y);
	     (if (h.p.y > !y_max) || (!y_max = -1.) then y_max := h.p.y);
	     (if (h.q.x < !x_min) || (!x_min = -1.) then x_min := h.q.x);
	     (if (h.q.x > !x_max) || (!x_max = -1.) then x_max := h.q.x);
	     (if (h.q.y < !y_min) || (!y_min = -1.) then y_min := h.q.y);
	     (if (h.q.y > !y_max) || (!y_max = -1.) then y_max := h.q.y);
	     aux t in
  aux forme
      
let polygone_regulier point radius nb_cotes =
  let pi = 4. *. atan 1. in
  let rec aux n res =
    if n = 0 then res
    else aux (n-1)
	     ({x= point.x +. (radius *. cos((2.*.(float_of_int (n-1))*.pi/.(float_of_int nb_cotes))+.pi));
	       y= point.y +. (radius *. sin((2.*.(float_of_int (n-1))*.pi/.(float_of_int nb_cotes))+.pi))}
	      :: res) in
  aux nb_cotes []
	     
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
  let box = create_box_polygone polygone in 
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
			   
(* direction correspond a la direction des droites remplissant le polygone
   direction vaut 0 si c'est horizontal, 1 si c'est vertical *)
let compare direction p1 p2 =
  if direction == 0 then compare_abscisse p1 p2 else compare_ordonnee p1 p2

let print_forme_inner environment forme direction shift_value =
  let segments = forme in
  let polygone_box = create_box_forme forme in
  let intersections = ref [] in
  let unretract = ref false in
  let rec aux pts res =
    match pts with
      [] -> unretract := false; res
     |e1::e2::e3::t ->
       if !unretract then
	 begin
	   unretract := true;
	   aux (e3::t) (G10(None)::(print_to environment e2.x e2.y)::G11(None)::(move_to environment e1.x e1.y)::res)
	 end
       else
	 begin
	   unretract := true;
	   aux (e3::t) (G10(None)::(print_to environment e2.x e2.y)::(move_to environment e1.x e1.y)::res)
	 end
     |e1::e2::t ->
       if !unretract then
	 aux t ((print_to environment e2.x e2.y)::G11(None)::(move_to environment e1.x e1.y)::res)
       else
	 aux t ((print_to environment e2.x e2.y)::(move_to environment e1.x e1.y)::res)
     |h::t ->
       if !unretract then
	 aux t ((print_point environment)::G11(None)::(move_to environment h.x h.y)::res)
       else
	 aux t ((print_point environment)::(move_to environment h.x h.y)::res)
  in
  let droite = ref {a=0.;b=0.;c=0.} in
  let taille_intervalle = if direction == 0 then (polygone_box.y_max-.polygone_box.y_min) else (polygone_box.x_max-.polygone_box.x_min) in
  let res = ref [] in
  let sens = ref 1 in
  let i = ref 0. in
  if direction == 0 then droite := {a=(-1.);b=0.;c=(-1.)*.polygone_box.y_min} else droite := {a=0.;b=1.;c=(-1.)*.polygone_box.x_min};
  while (!i < taille_intervalle) || ((=.) !i taille_intervalle)
  do
    intersections := List.sort_uniq
		       (fun p1 p2 -> !sens * (compare direction p1 p2))
		       (intersections_segments_droite segments !droite);
    res := List.append (aux !intersections []) !res;
    intersections := [];
    droite := {a=(!droite).a;b=(!droite).b;c=(!droite).c-.shift_value};
    sens := !sens * -1;
    i := !i +. shift_value
  done;
  !res
			   
let print_polygone_inner environment polygone direction shift_value =
  print_forme_inner environment (polygone_to_segments polygone) direction shift_value
