open Point
open Environment
open Program
open Polygone
open Forme
       
let _ =
  let file = open_out "out/cone.gcode" in
  let env = environment_init in
  let centre = {x=50.;y=50.} in
  let radius = 10. in
  let cercle = polygone_regulier centre radius 30 in
  let forme = polygone_to_segments cercle in
  let pente = env.extruder_radius in
  let couches = int_of_float(2. *. radius /. pente) in
  let prog = print_forme_recursive env (fun f -> resize_forme f (-1. *. env.extruder_radius) (-1. *. env.extruder_radius)) forme couches in
  print file (program_init env);
  print file prog;
  print file (program_end env)
