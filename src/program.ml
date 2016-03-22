open Environment
open Instruction
open Point
open Printf

(*
Primitives
 *)

type t = instruction list

let add_instruction instr program = instr :: program
       
let move_to env x y =
  let dist = distance {x=x;y=y} env.pos in
  env.printing <- false;
  env.pos.x <- x;
  env.pos.y <- y;
  env.time <- env.time +. (dist/.env.speed_G0);
  G0_move (x,y)

let print_to env x y =
  let dist = distance {x=x;y=y} env.pos in
  let d = dist *. (get_distance_plastic_rate env 1) in
  env.printing <- true;
  env.pos.x <- x;
  env.pos.y <- y;
  env.time <- env.time +. (dist/.env.speed_G1);
  env.plastic <- env.plastic +. d;
  env.total_plastic <- env.total_plastic +. d;
  G1_print(x,y, env.plastic)      

let print_point env =
  let d = env.extruder_radius *. (get_distance_plastic_rate env 1) in
  env.plastic <- env.plastic +. d;
  env.total_plastic <- env.total_plastic +. d;
  G1_print(env.pos.x,env.pos.y, env.plastic)
  
	  
let move env x y =
  move_to env (env.pos.x+.x) (env.pos.y+.y)
		 
let print env x y =
  print_to env (env.pos.x+.x) (env.pos.y+.y)

	   
let lift_to env e =
  env.height <- e;
  env.layer <- env.layer + 1;
  G0_height(e)
	   
let lift env =
  lift_to env (env.height+.env.lift_step)
	   
<<<<<<< HEAD
let comment_layer env=
  Comment(sprintf "LAYER: %d" env.layer)
=======
let inline_retract program = program
  (* let rec aux p res= *)
  (*   match p with *)
  (*   [] -> res *)
  (*   |(G1_print _ as g1)::(G0_move _ as g0)::t *)
  (*   |(G1 _ as g1)::(G0  _ as g0)::t *)
  (*   |(G1_print _ as g1)::(G0  _ as g0)::t *)
  (*   |(G1 _ as g1)::(G0_move  _ as g0)::t *)
  (*     -> aux (g0::t) (G10(None) :: g1 ::res)  *)
    
  (*   |(G0 _ as g0)::(G1  _ as g1)::t *)
  (*   |(G0 _ as g0)::(G1_print  _ as g1)::t *)
  (*   |(G0_move _ as g0)::(G1  _ as g1)::t *)
  (*   |(G0_move _ as g0)::(G1_print _ as g1) ::t -> *)
  (*     aux (g1::t) (G11(None) :: g0 :: res) *)

  (*   |h::t-> aux t (h::res) in *)
  (* aux program [] |> List.rev  *)
>>>>>>> 9087f4e0e71375b05a51cdb534b3b84b8d63e7e9
	   
let program_init env =
  G0_height(env.height_init)::Comment("LAYER: 0")::Comment(sprintf "Layer count: %d" env.layer)::Comment(sprintf "TIME: %.0f" env.time)::G92_init(0.)::G1_speed(env.speed_G1)::G0_speed(env.speed_G0)::M106_fan_speed(env.fan_speed)::Comment("This is a comment")::[]

let program_end env =
  M107::G10(None)::G28("XY")::G0_height(env.height +. 1.)::[]

let print fmt program =
  let rec to_string = function
    | [] -> ()
    | h::t-> Printf.fprintf fmt "%s" (instruction_to_string h);
      to_string t 
  in to_string (inline_retract (List.rev program))
