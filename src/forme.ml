open Instruction
open Point
open Environment
open Program
open Droite
open Segment
open Box
open Printf

type forme = segment list
       
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

let barycentre_forme forme =
  let x_sum = ref 0. in
  let y_sum = ref 0. in
  let rec aux p n=
    match p with
      [] -> {x= !x_sum /. (float_of_int (2*n)); y= !y_sum /. (float_of_int (2*n))}
     |h::t-> x_sum:= !x_sum +. h.p.x +. h.q.x;
	     y_sum:= !y_sum +. h.p.y +. h.q.y;
	     aux t (n+1) in
  aux forme 0

let resize_forme forme w h =
  let box = create_box_forme forme in
  let point_barycentre = barycentre_forme forme in
  let width_pourcentage = w/.(box.x_max -. box.x_min) in
  let height_pourcentage = h/.(box.y_max -. box.y_min) in
  let rec aux p res =
    match p with
      [] -> List.rev res
     |h::t->
       aux t ((create_segment
		 {x=h.p.x -. (point_barycentre.x -. h.p.x) *. width_pourcentage; y=h.p.y -. (point_barycentre.y -. h.p.y) *. height_pourcentage}
		 {x=h.q.x -. (point_barycentre.x -. h.q.x) *. width_pourcentage; y=h.q.y -. (point_barycentre.y -. h.q.y) *. height_pourcentage})
		   ::res) in
  aux forme []

let print_forme_current_place evironment forme =
  "chantier"

let print_forme environment forme =
  let reverse_forme = List.rev forme in
  let rec to_instructions f res =
    match f with
      [] -> res
     |h::t->
       if ((=.) h.p.x environment.pos.x) && ((=.) h.p.y environment.pos.y) then
	 to_instructions t ((print_to environment h.q.x h.q.y)::res)
       else
	 if ((=.) h.q.x environment.pos.x) && ((=.) h.q.y environment.pos.y) then
	   to_instructions t ((print_to environment h.p.x h.p.y)::res)
	 else
	   to_instructions t ((print_to environment h.q.x h.q.y)::(move_to environment h.p.x h.p.y)::res)
  in
  to_instructions reverse_forme []
      
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

let print_forme_monotone environment forme couches =
  let petite_forme = resize_forme forme (-4.*.environment.extruder_radius) (-4.*.environment.extruder_radius) in
  let a_remplir = resize_forme petite_forme (-4.*.environment.extruder_radius) (-4.*.environment.extruder_radius) in
  let direction = ref 0 in
  let rec to_instructions couche res=
    if (couche == 0)
    then
      res
    else
      begin
	direction := (!direction + 1) mod 2; 
	if ((couche == 1) || (couche==2) || (couche==couches) || (couche==(couches-1)))
	   then
	     begin
	       let simple_forme = List.append (print_forme environment forme) res in
	       let double_forme = List.append (print_forme environment petite_forme) simple_forme in
	       let base_remplie = List.append (print_forme_inner environment a_remplir !direction (2.*.environment.extruder_radius)) double_forme in
	       to_instructions (couche-1) ((lift environment)::(comment_layer environment)::base_remplie)
	     end
	    else
	      begin
		let simple_forme = List.append (print_forme environment forme) res in
	        let gaufrage = List.append (print_forme_inner environment petite_forme !direction environment.gaufrage) simple_forme in
		to_instructions (couche-1) ((lift environment)::(comment_layer environment)::gaufrage)
	      end
      end
  in
  to_instructions couches []
	
let print_forme_recursive environment func forme couches =
  let direction = ref 0 in
  let rec to_instructions f couche res=
    if (couche == 0)
    then
      res
    else
      begin
	let petite_forme = resize_forme f (-4.*.environment.extruder_radius) (-4.*.environment.extruder_radius) in
	direction := (!direction + 1) mod 2; 
	if ((couche == 1) || (couche==2) || (couche==couches) || (couche==(couches-1)))
	   then
	     begin
	       let a_remplir = resize_forme petite_forme (-4.*.environment.extruder_radius) (-4.*.environment.extruder_radius) in
	       let simple_forme = List.append (print_forme environment f) res in
	       let double_forme = List.append (print_forme environment petite_forme) simple_forme in
	       let base_remplie = List.append (print_forme_inner environment a_remplir !direction (2.*.environment.extruder_radius)) double_forme in
	       to_instructions (func f) (couche-1) ((lift environment)::(comment_layer environment)::base_remplie)
	     end
	    else
	      begin
		let simple_forme = List.append (print_forme environment f) res in
	        let gaufrage = List.append (print_forme_inner environment petite_forme !direction environment.gaufrage) simple_forme in
		to_instructions (func f) (couche-1) ((lift environment)::(comment_layer environment)::gaufrage)
	      end
      end
  in
  to_instructions forme couches []


let print_forme_iterative environment func forme couches =
  let direction = ref 0 in
  let rec to_instructions f couche res=
    if (couche >= couches)
    then
      res
    else
      begin
	let petite_forme = resize_forme f (-4.*.environment.extruder_radius) (-4.*.environment.extruder_radius) in
	direction := (!direction + 1) mod 2; 
	if ((couche == 1) || (couche==2) || (couche==couches) || (couche==(couches-1)))
	   then
	     begin
	       let a_remplir = resize_forme petite_forme (-4.*.environment.extruder_radius) (-4.*.environment.extruder_radius) in
	       let simple_forme = List.append (print_forme environment f) res in
	       let double_forme = List.append (print_forme environment petite_forme) simple_forme in
	       let base_remplie = List.append (print_forme_inner environment a_remplir !direction (2.*.environment.extruder_radius)) double_forme in
	       to_instructions (func couche f) (couche+1) ((lift environment)::(comment_layer environment)::base_remplie)
	     end
	    else
	      begin
		let simple_forme = List.append (print_forme environment f) res in
	        let gaufrage = List.append (print_forme_inner environment petite_forme !direction environment.gaufrage) simple_forme in
		to_instructions (func couche f) (couche+1) ((lift environment)::(comment_layer environment)::gaufrage)
	      end
      end
  in
  to_instructions forme couches []		  

let print_forme_recursive_clean environment func forme couches =
  let rec to_instructions f couche res=
    if (couche == 0)
    then
      res
    else
      to_instructions (func f) (couche - 1) (List.append (print_forme environment f) ((lift environment)::res))
  in
  to_instructions forme couches []
