open Instruction
open Point
open Environment
open Program
open Polygone

let _ =
  let file = open_out "out/boulon.gcode" in
  let env = environment_init in
  let hauteur = 10.3 in
  let centre = {x=50.;y=50.} in
  let cercle = polygone_regulier centre 10. 20 in
  let hexagone = polygone_regulier centre 20. 6 in
  let petit_hexagone = resize hexagone (-2.*.env.extruder_radius) (-2.*.env.extruder_radius) in
  let grand_cercle = resize cercle (2.*.env.extruder_radius) (2.*.env.extruder_radius) in
  let petite_forme = List.append (polygone_to_segments petit_hexagone) (polygone_to_segments grand_cercle) in
  let forme_interieur = List.append (polygone_to_segments (resize petit_hexagone (-2.*.env.extruder_radius) (-2.*.env.extruder_radius)))
				 (polygone_to_segments (resize grand_cercle (2.*.env.extruder_radius) (2.*.env.extruder_radius))) in
  let direction = ref 0 in
  print file (program_init env);
  while (env.height < hauteur) || ((=.) env.height hauteur)
  do
    (if ((=.) env.height 0.3) || ((=.) env.height 0.4) || ((=.) env.height hauteur) || ((=.) env.height (hauteur-.0.1)) then
       begin
	 (if ((=.) env.height (hauteur-.0.1)) then print file (M106_fan_speed(255)::[]));
	 print file (print_polygone env hexagone);
	 print file (print_polygone env petit_hexagone);
	 print file (print_polygone env grand_cercle);
	 print file (print_polygone env cercle);
	 print file (print_forme_inner env forme_interieur !direction (1.5*.env.extruder_radius))
       end
     else
       begin
	 let _ = print file (print_polygone env hexagone) in
	 let _ = print file (print_polygone env cercle) in
	 print file (print_forme_inner env petite_forme !direction 6.)
       end
    );
    print file ((lift env)::[]);
    direction := (!direction +1) mod 2
  done;
  print file (program_end env)
