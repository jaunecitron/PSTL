open Instruction
open Point
open Environment
open Program
open Polygone

let _ =
  let env = environment_init in
  let pente = -1. *. env.extruder_radius in
  let hauteur = 10.3 in
  let direction = ref 0 in
  let centre = {x=50.;y=50.} in
  let pyramide = ref (polygone_regulier centre 20. 8) in
  let petite_pyramide = ref [] in
  let file = open_out "out/pyramide.gcode" in
  let res = ref [] in
  while (env.height < hauteur) || ((=.) env.height hauteur)
  do
    res := List.append (print_polygone env !pyramide) !res;
    petite_pyramide := (resize_polygone !pyramide (2.*.pente) (2.*.pente));
    (if ((=.) env.height 0.3) || ((=.) env.height 0.4) || ((=.) env.height hauteur) || ((=.) env.height (hauteur-.0.1)) then
       begin
	 (if ((=.) env.height (hauteur-.0.1)) then print file (M106_fan_speed(255)::[]));
	 res := List.append (print_polygone env !petite_pyramide) !res;
        res := List.append (print_polygone_inner env (resize_polygone !petite_pyramide (-4.*.env.extruder_radius) (-4.*.env.extruder_radius)) !direction (1.5*.env.extruder_radius)) !res;
      end
    else
      res := List.append (print_polygone_inner env !petite_pyramide !direction 6.) !res
    );
    res := (lift env)::(comment_layer env)::!res;
    direction := (!direction + 1) mod 2;
    pyramide := !petite_pyramide
  done;
  print file (program_init env);
  print file !res;
  print file (program_end env)
  
