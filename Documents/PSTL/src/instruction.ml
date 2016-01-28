open Printf

type instruction =
  | G0 of (float * float * float * float * float * int)
  | G0_move of (float * float) (* G0(x,y) rapid move without extrude *)
  | G0_speed of (float)
  | G0_height of (float) (* G0_height(z) move to height z*)
  | G1 of (float * float * float * float * float * float)(* G1(x,y,e) move with extrude *)
  | G1_print of (float * float * float)
  | G1_speed of (float)
  | M107
      
let instruction_to_string instr=
  match instr with
  | G0 (x,y,z,e,f,s) -> sprintf "G0 X%.4f Y%.4f Z%.4f E%f F%f S%d\n" x y z e f s
  | G0_move (x,y) -> sprintf "G0 X%.4f Y%.4f\n" x y
  | G0_speed (f) -> sprintf "G0 F%f\n" f
  | G0_height(z) -> sprintf "G0 Z%.4f\n" z
  | G1 (x,y,z,e,f,s) -> sprintf "G1 X%.4f Y%.4f Z%.4f E%.4f F%.4f S%.4f\n" x y z e f s
  | G1_print  (x,y,e) -> sprintf "G1 X%.4f Y%.4f E%f\n" x y e
  | G1_speed (f) -> sprintf "G1 F%f\n" f
  | M107 -> sprintf "M107\n"
