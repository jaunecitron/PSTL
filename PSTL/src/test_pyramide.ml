open Point
open Environment
open Program
open Polygone

let _ =
  let p1 = {x=60.;y=50.} in
  let p2 = {x=55.;y=58.66} in
  let p3 = {x=45.;y=58.66} in
  let p4 = {x=40.;y=50.} in
  let p5 = {x=45.;y=41.34} in
  let p6 = {x=55.;y=41.34} in
  let pyramide = p6::p5::p4::p3::p2::p1::[] in
  let petite_pyramide = resize pyramide 10. 10. in
  let env = environment_init in
  let file = open_out "out/pyramide.gcode" in
  print file (print_polygone env petite_pyramide)
