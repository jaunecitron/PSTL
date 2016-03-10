open Point
open Environment
open Program
open Polygone

let _ =
  let env = environment_init in
  let pente = -0.1 in
  let hauteur = 15.3 in
  let direction = ref 0 in
  let pyramide = ref (polygone_regulier {x=50.;y=50.} 10. 6) in
  let petite_pyramide = ref [] in
  let file = open_out "out/pyramide.gcode" in
  print file (program_init env);
  while (env.height < hauteur) || ((=.) env.height hauteur)
  do
    if ((=.) env.height 0.3) || ((=.) env.height 0.4) || ((=.) env.height hauteur) || ((=.) env.height (hauteur-.0.1)) then
      begin
	petite_pyramide := (resize !pyramide pente pente);
	print file (print_polygone env !pyramide);
	print file (print_polygone env !petite_pyramide);
	print file (print_polygone_inner env (resize !petite_pyramide (2.*.env.extruder_radius) (2.*.env.extruder_radius)) !direction env.extruder_radius);
	pyramide := !petite_pyramide
      end
    else
      begin
	print_endline "chantier"
      end
  done;
  print file (program_end env)
  
