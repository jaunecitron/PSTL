open Program
open Environment
open Point
open Polygone
open Forme

let _ =
  let env = environment_init in
  let prog = ref [] in
  let file = open_out "out/cube.gcode" in
  let carre = polygone_regulier {x=50.;y=50.} 5. 4 in
  let couches = 30 in
  let forme = polygone_to_segments carre in
  prog := print_forme_monotone env forme couches;
  print file (program_init env);
  print file !prog;
  print file (program_end env)
