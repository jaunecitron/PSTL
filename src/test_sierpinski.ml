open Point
open Environment
open Program
open Segment
open Polygone
open Forme

exception SomethingWrong
       
let norme vecteur = sqrt(vecteur.x*.vecteur.x +. vecteur.y*.vecteur.y)
let reverse vecteur = {x= -1.*.vecteur.x; y= -1.*.vecteur.y}
let resize vecteur taille =
  let pourcentage = taille/.(norme vecteur) in
  {x=vecteur.x*.pourcentage; y=vecteur.y*.pourcentage} 
let angle_vecteur_abscisse vecteur =
  let res = acos(vecteur.y/.(norme vecteur)) in
  if vecteur.y < 0. then
    -1.*.res
  else
    res
      
let sierpinski environment triangle longueur_cote =
  let rec aux triangles res =
    match triangles with
    | [] -> res
    | a::b::c::t ->
       if ((distance a b) > longueur_cote) then
	 let ba = resize {x=a.x-.b.x;y=a.y-.b.y} (environment.extruder_radius) in
	 let cb = resize {x=b.x-.c.x;y=b.y-.c.y} (environment.extruder_radius) in
	 let ac = resize {x=c.x-.a.x;y=c.y-.a.y} (environment.extruder_radius) in
	 let p1 = {x=(a.x+.b.x)/.2.; y=(a.y+.b.y)/.2.} in
	 let p2 = {x=(b.x+.c.x)/.2.; y=(b.y+.c.y)/.2.} in
	 let p3 = {x=(c.x+.a.x)/.2.; y=(c.y+.a.y)/.2.} in
	 let t1p = {x= p1.x +. ba.x; y= p1.y +. ba.y} in
	 let t1q = {x= p3.x -. ac.x; y= p3.y -. ac.y} in
	 let t2p = {x= p2.x +. cb.x; y= p2.y +. cb.y} in
	 let t2q = {x= p1.x -. ba.x; y= p1.y -. ba.y} in
	 let t3p = {x= p3.x +. ac.x; y= p3.y +. ac.y} in
	 let t3q = {x= p2.x -. cb.x; y= p2.y -. cb.y} in
	 if ((distance p1 p2) > longueur_cote) then
	   aux (t3q::t3p::c::t2q::t2p::b::t1q::t1p::a::t) ((create_segment p1 p2)::(create_segment p3 p1)::(create_segment p2 p3)::res)
	 else
	   aux (t3q::t3p::c::t2q::t2p::b::t1q::t1p::a::t) res
       else
	 aux t ((create_segment c a)::(create_segment b c)::(create_segment a b)::res)
    | _ -> raise SomethingWrong
  in
  aux triangle []

let transformation environment sierpinski pente =
  let rec aux forme res =
    match forme with
    | [] -> res
    | a::b::c::t ->
       let cote = distance a.p a.q in
       if ((cote > environment.extruder_radius) || ((=.) cote environment.extruder_radius)) then
	 aux t (List.append (resize_forme (a::b::c::[]) (-1.*.pente) (-1.*.pente)) res)
       else
	 aux t res
    | _ -> raise SomethingWrong
  in
  aux sierpinski []

let nb_couches sierpinski pente=
  match sierpinski with
  | [] -> 0
  | h::t -> int_of_float (((distance h.p h.q)/.2.)/.pente)
      
let _ =
  let file = open_out "out/sierpinski.gcode" in
  let env = environment_init in
  let centre = {x=50.111;y=50.111} in
  let triangle = polygone_regulier centre 20. 3 in
  let forme = sierpinski env triangle 3. in
  let pente = env.extruder_radius in
  let couches = nb_couches (polygone_to_segments triangle) pente in
  let prog = print_forme_recursive_clean env (fun f -> transformation env f pente) forme couches in
  print_endline (Printf.sprintf "Il va y avoir %d couches" couches);
  print file (program_init env);
  print file prog;
  print file (program_end env)

	(*Objectif
print_forme_recursive env (fun f -> transformation env f pente) forme couches*)
