open Printf

(*
 Type env
*)
type env=
  {
    mutable x : float;
    mutable y : float;
    mutable positionning : int; (* 0 to absolute; 1 to relative *)
    mutable plastic : float;
    mutable total_plastic : float;
  }
    
let init =
  {
    x=0.;
    y=0.;
    positionning=0;
    plastic=0.;
    total_plastic=0.
  }
  
let env_to_string env=
  sprintf "env = { x=%f ; y=%f ; positionning=%d ; plastic=%f ; total_plastic=%f }"
    env.x env.y env.positionning env.plastic env.total_plastic


(*
Type instr
*)
type instr =
  | G0 of (float * float) (* G0(x,y) rapid move without extrude *)
  | G1 of (float * float * float option) (* G1(x,y,e) move with extrude *)

exception InstrWrongType
      
let instr_to_string instr=
  match instr with
  | G0 (x,y) -> sprintf "G0 X%.2f Y%.2f\n" x y
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
let moveto env p x y =
  env.x <- x;
  env.y <- y;
  add_instr (G0 (x,y)) p

let printto env p x y =
  env.x <- x;
  env.y <- y;
  add_instr (G1 (x,y,None)) p
    
let move env p x y =
  env.x <- env.x +. x;
  env.y <- env.y +. y;
  add_instr (G0 (env.x,env.y)) p

let print env p x y =
  env.x <- env.x +. x;
  env.y <- env.y +. y;
  add_instr (G1 (env.x,env.y,None)) p
	    
let program_to_string p =
  let rec to_string l =
    match l with
      | [] -> ""
      | h::t -> (instr_to_string h) ^ to_string t
  in to_string p

  
(*
Main
*)
let () =
  let cur_env = init in
  let p = moveto cur_env program 20.0 20.0 in
  print_string(program_to_string p)
  
