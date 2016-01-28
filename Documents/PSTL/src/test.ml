open Program
open Environment
open Point
open Polygone


let _ =
  let p1 = {x=0.0;y=0.0} in
  let p2 = {x=0.0;y=10.0} in
  let p3 = {x=10.0;y=10.0} in
  let p4 = {x=10.0;y=0.0} in
  let polygone = p4::p3::p2::p1::[] in
  let tmp = ref 0 in
  let cur_env = env_init in
  let prog = ref (program_init cur_env) in
  prog := (move_to cur_env 20.0 20.0) :: !prog;
  while cur_env.pos.x<=30.0
  do
    if (!tmp mod 2) = 0 then
      begin
	prog := (print_to cur_env cur_env.pos.x 30.0) :: !prog;
	prog := (print_to cur_env (cur_env.pos.x+.0.1) 30.0) :: !prog;
	tmp := !tmp + 1
      end
    else
      begin
	prog := (print_to cur_env cur_env.pos.x 20.0) :: !prog;
	prog := (print_to cur_env (cur_env.pos.x+.0.1) 20.0) :: !prog;
	tmp := !tmp + 1
      end
  done;
  prog := (print_to cur_env 30. 30.) ::  !prog;
  prog := (lift cur_env) :: !prog;
  prog := (move_to cur_env 20. 20.) :: !prog;
  while cur_env.height <= 10.3
  do
    prog := List.append (print_polygone cur_env polygone) !prog;
    prog := (lift cur_env) :: !prog
  done;
  tmp:=0;
  while cur_env.pos.x<=30.0
  do
    if (!tmp mod 2) = 0 then
      begin
	prog := (print_to cur_env cur_env.pos.x 30.0) :: !prog;
	prog := (print_to cur_env (cur_env.pos.x+.0.1) 30.0) :: !prog;
	tmp := !tmp + 1
      end
    else
      begin
	prog := (print_to cur_env cur_env.pos.x 20.0) :: !prog;
	prog := (print_to cur_env (cur_env.pos.x+.0.1) 20.0) :: !prog;
	tmp := !tmp + 1
      end
  done;
  prog := (print_to cur_env 30. 30.) ::  !prog;
  prog := (lift cur_env) :: !prog;
  print_string (program_to_string !prog)
    
    
      
  
       
       (*
let _ =
  let cur_env = env_init in
  let prog = add_instruction (move_to cur_env 20.0 20.0) [] in
  print_string (program_to_string prog);
  let p1 = {x=0.0;y=0.0} in
  let p2 = {x=0.0;y=10.0} in
  let p3 = {x=10.0;y=10.0} in
  let p4 = {x=10.0;y=0.0} in
  let polygone = p4::p3::p2::p1::[] in
  let prog_final = ref prog in
  while cur_env.height < 10. do
    prog_final := List.append (print_polygone cur_env polygone) !prog_final;
    prog_final := add_instruction (lift cur_env) !prog_final;
    (*print_string (env_to_string cur_env);*)
  done;
  print_string (program_to_string !prog_final)
	*)


