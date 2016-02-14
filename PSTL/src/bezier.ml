open Point
open Printf
open Program

let factorielle n =
  let rec fact i res =
    if i = 0 then (1*res) else (fact (i-1) (res*i)) in
  fact n 1

let coefficient_binomial k n =
  (factorielle n)/((factorielle k) * (factorielle (n-k)))
		    
let polynome_bernstein i n t =
  (float_of_int (coefficient_binomial i n)) *. (t**(float_of_int i)) *. ((1.-.t)**(float_of_int (n-i)))

let bezier t l =
  let n = (List.length l) - 1 in
  let res_x = ref 0. in
  let res_y = ref 0. in
  let i = ref 0 in
  let rec bez l =
    match l with
       [] -> ()
      |head::tail-> res_x := (polynome_bernstein !i n t) *. head.x +. !res_x;
	      res_y := (polynome_bernstein !i n t) *. head.y +. !res_y;
	      i := !i + 1;
	      bez tail in
  bez l
									  
let _ =
  print_string (sprintf "C(3 8) %d\n" (coefficient_binomial 3 8));
  print_string (sprintf "3! %d\n" (factorielle 3));
  print_string (sprintf "%f\n" (3.**5.));
  
