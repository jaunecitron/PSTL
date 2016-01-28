open Program
open Environment
open Point
open Polygone

let _ =
  let cur_env = env_init in
  let prog = add_instruction (move_to cur_env 20.0 20.0) [] in
  print_string (program_to_string prog);
  let p1 = {x=0.0;y=0.0} in
  let p2 = {x=0.0;y=10.0} in
  let p3 = {x=10.0;y=10.0} in
  let p4 = {x=10.0;y=0.0} in
  let polygone = p1::p2::p3::p4::[] in
  let prog_final = ref prog in
  while cur_env.height < 10. do
    prog_final := List.append (print_polygone cur_env polygone) !prog_final;
    prog_final := add_instruction (lift cur_env) !prog_final;
    (*print_string (env_to_string cur_env);*)
  done;
  print_string (program_to_string !prog_final)

