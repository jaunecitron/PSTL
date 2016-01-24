open Printf

open Point

(*
Type polygone
 *)
type polygone = point list

let add p polygone =
  let rec add l =
    match l with
      [] -> p :: []
     |h::t-> h :: add t
  in add polygone

let polygone_to_string p =
  let rec to_string l =
    match l with
      [] -> "\n"
     |h::t-> (point_to_string h) ^";"^ (to_string t) in
  to_string p
       
(*
 Type env
*)
type env=
  {
    mutable pos : point;
    mutable positionning : int; (* 0 to absolute; 1 to relative *)
    mutable height : float;
    mutable height_step : float;
    mutable plastic : float;
    mutable total_plastic : float;
  }
    
let init =
  {
    pos={x=0.;y=0.};
    positionning=0;
    height=0.;
    height_step=1.;
    plastic=0.;
    total_plastic=0.
  }
  
let env_to_string env=
  sprintf "env = {\n pos=%s ;\n positionning=%d ;\n height = %f ;\n height_step = %f ;\n plastic=%f ;\n total_plastic=%f }\n"
    (point_to_string env.pos) env.positionning env.height env.height_step env.plastic env.total_plastic


(*
Type instr
*)
type instr =
  | G0 of (float * float) (* G0(x,y) rapid move without extrude *)
  | G0_height of (float) (* G0_height(z) move to height z*)
  | G1 of (float * float * float option) (* G1(x,y,e) move with extrude *)

exception InstrWrongType
      
let instr_to_string instr=
  match instr with
  | G0 (x,y) -> sprintf "G0 X%.2f Y%.2f\n" x y
  | G0_height(z) -> sprintf "G0 Z%.2f\n" z
  | G1 (x,y,None) -> sprintf "G1 X%.2f Y%.2f\n" x y
  | G1 (x,y,Some e) -> sprintf "G1 X%.2f Y%.2f E%.2f\n" x y e

(*
Type program
*)
let program = []

let add_instr instr p=
  let rec add l =
    match l with
      [] -> instr :: []
    |h::t-> h :: add t
  in add p


(*
Primitives
*)
let move_to env x y =
  env.pos.x <- x;
  env.pos.y <- y;
  G0 (x,y)

let print_to env x y =
  env.pos.x <- x;
  env.pos.y <- y;
  G1 (x,y,None)      
    
let move env x y =
  env.pos.x <- env.pos.x +. x;
  env.pos.y <- env.pos.y +. y;
  G0 (env.pos.x,env.pos.y)

let print env x y =
  env.pos.x <- env.pos.x +. x;
  env.pos.y <- env.pos.y +. y;
  G1 (env.pos.x,env.pos.y,None)

let lift env =
  env.height <- env.height +. env.height_step;
  G0_height(env.height)
	   
let lift_to env e =
  env.height <- e;
  G0_height(e)
	   
let program_to_string p =
  let rec to_string l =
    match l with
      | [] -> ""
      | h::t -> (instr_to_string h) ^ to_string t
  in to_string p

exception EmptyArgument
	       
let print_polygone env l =
  match l with
    [] -> raise EmptyArgument
   |h::t->  let x = h.x in
	   let y = h.y in
	   let init_x = env.pos.x in
	   let init_y = env.pos.y in
	   let rec add_cote l =
	     match l with
	       [] -> print_string(sprintf "Point env initial (%.2f,%.2f)\n" init_x init_y);(print_to env init_x init_y) :: [] 
	     |h::t ->(print_to env (init_x+.x-.h.x) (init_y +.y-.h.y)) :: add_cote t 
	   in let tmp = add_cote t in
	      env.pos.x <- init_x;
	      env.pos.y <- init_y;
	      tmp

let print_polygone_2 env p =
  match p with
    [] -> raise EmptyArgument
  |h::t->  let x = h.x in
	   let y = h.y in
	   let init_x = env.pos.x in
	   let init_y = env.pos.y in
	   let rec add_cote l =
	     match l with
	       [] -> G1(init_x,init_y,None) :: []
	      |h::t-> G1(init_x+.h.x-.x,init_y+.h.x-.y,None) :: add_cote t in
	   add_cote t
									 
(*
Main
*)
let _ =
  let cur_env = init in
  let prog = add_instr (move_to cur_env 20.0 20.0) [] in
  let p1 = {x=0.0;y=0.0} in
  let p2 = {x=0.0;y=10.0} in
  let p3 = {x=10.0;y=10.0} in
  let p4 = {x=10.0;y=0.0} in
  let polygone = p1::p2::p3::p4::[] in
  print_string (env_to_string cur_env);
  let prog_final = ref prog in
  while cur_env.height < 10. do
    prog_final := List.append !prog_final (print_polygone cur_env polygone);
    prog_final := add_instr (lift cur_env) !prog_final;
    print_string (env_to_string cur_env);
  done;
  print_string (program_to_string !prog_final)


  
