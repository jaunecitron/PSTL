open Point
open Environment
open Program
open Polygone
open Forme

let _ =
  let file = open_out "out/cylindre.gcode" in
  let centre = {x=50.;y=50.} in
  let cercle = polygone_regulier centre 10. 20 in
  let env = environment_init in
  let program = print_polygone_monotone env cercle 10 in
  print file (program_init env);
  print file program;
  print file (program_end env)