open Instruction
open Point
open Environment
open Program
open Polygone
open Forme

let _ =
  let file = open_out "out/pyramide.gcode" in
  let env = environment_init in
  let centre = {x=50.;y=50.} in
  let pyramide = polygone_regulier centre 20. 8 in
  let forme = polygone_to_segments pyramide in
  let prog = print_forme_recursive_clean env (fun f -> resize_forme f (-1.*.env.extruder_radius) (-1.*.env.extruder_radius)) forme 100 in
  print file (program_init env);
  print file prog;
  print file (program_end env)
