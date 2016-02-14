open Environment
open Instruction
open Point
open Printf

(*
Primitives
 *)

let add_instruction instr program = instr :: program
       
let move_to env x y =
  env.printing <- false;
  env.pos.x <- x;
  env.pos.y <- y;
  G0_move (x,y)

let print_to env x y =
  let d = (distance {x=x;y=y} env.pos) *. (get_distance_plastic_rate env 1) in
  env.printing <- true;
  env.pos.x <- x;
  env.pos.y <- y;
  env.plastic <- env.plastic +. d;
  env.total_plastic <- env.total_plastic +. d;
  G1_print(x,y, env.plastic)      
    
let move env x y =
  env.printing <- false;
  env.pos.x <- env.pos.x +. x;
  env.pos.y <- env.pos.y +. y;
  G0_move (env.pos.x,env.pos.y)

let print env x y =
  let d = (distance {x=env.pos.x +. x;y=env.pos.y +. y} env.pos) *. (get_distance_plastic_rate env 1) in
  env.printing <- true;
  env.pos.x <- env.pos.x +. x;
  env.pos.y <- env.pos.y +. y;
  env.plastic <- env.plastic +. d;
  env.total_plastic <- env.total_plastic +. d;
  G1_print (env.pos.x,env.pos.y,env.plastic)

let lift env =
  env.height <- env.height +. env.lift_step;
  G0_height(env.height)
	   
let lift_to env e =
  G0_height(e)
	   
let program_init env =
  G0_height(env.height)::G1_speed(env.speed_G1)::G0_speed(env.speed_G0)::M106_fan_speed(env.fan_speed)::Comment("This is a comment")::[]

let program_end env =
  M107::G10(None)::G28("XY")::G0_height(env.height +. 1.)::[]

let program_to_string program =
  let rec to_string p res =
    match p with
      [] -> res
     |h::t-> let instr = instruction_to_string h in
	     to_string t (instr^res) in
  to_string program ""
