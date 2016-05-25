open Printf

type instruction =
  | G0 of (float * float * float * float * float * int) 
  | G0_move of (float * float) (* G0(x,y) rapid move without extrude *)
  | G0_speed of (float)
  | G0_height of (float) (* G0_height(z) move to height z*)
  | G1 of (float * float * float * float * float * int)(* G1(x,y,e) move with extrude *)
  | G1_print of (float * float * float)
  | G1_speed of (float)
  | G10 of (int option) (* G10 retract *)
  | G11 of (int option) (* G11 retract/unretract*)
  | G28 of (string)  (* G28(x,y,z) move to origin *)
  | G92 of (float * float * float * float) (* G92(x,y,z,e) allow programming of absolute zero point *)
  | G92_init of (float) (* G92(e) *)
  | M106_fan_speed of (int) (* M106(s) turn fan 0 at speed s *)
  | M107
  | Comment of (string)
      
let instruction_to_string instr=
  match instr with
  | G0 (x,y,z,e,f,s) -> sprintf "G0 X%.4f Y%.4f Z%.4f E%f F%f S%d\n" x y z e f s
  | G0_move (x,y) -> sprintf "G0 X%.4f Y%.4f\n" x y
  | G0_speed (f) -> sprintf "G0 F%f\n" f
  | G0_height(z) -> sprintf "G0 Z%.4f\n" z
  | G1 (x,y,z,e,f,s) -> sprintf "G1 X%.4f Y%.4f Z%.4f E%.4f F%.4f S%d\n" x y z e f s
  | G1_print  (x,y,e) -> sprintf "G1 X%.4f Y%.4f E%f\n" x y e
  | G1_speed (f) -> sprintf "G1 F%f\n" f
  | G10(None) -> sprintf "G10\n"
  | G10(Some s) -> sprintf "G10 S%d\n" s
  | G11(None) -> sprintf "G11\n"
  | G11(Some s) -> sprintf "G11 S%d\n" s
  | G28(s) -> let res = ref "G28" in
	      for i = 0 to ((String.length s) - 1) do res := !res ^ (sprintf " %c" s.[i]) done;
	      !res ^ "\n"
  | G92(x,y,z,e) -> sprintf "G28 X%.4f Y%.4f Z%.4f E%.4f\n" x y z e
  | G92_init(e) -> sprintf "G92 E%.4f\n" e
  | M106_fan_speed(s) -> sprintf "M106 S%d\n" s
  | M107 -> sprintf "M107\n"
  | Comment(s) -> sprintf ";%s\n" s
