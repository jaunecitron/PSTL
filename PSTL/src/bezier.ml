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
  let res = {x=0.;y=0.} in
  let i = ref 0 in							
  let rec bez l =
    match l with
       [] -> res
      |head::tail-> res.x <- (polynome_bernstein !i n t) *. head.x +. res.x;
		    res.y <- (polynome_bernstein !i n t) *. head.y +. res.y;
		    i := !i + 1;
		    bez tail in
  bez l
				 
    
let _ =
  print_string (sprintf "C(3 8) %d\n" (coefficient_binomial 3 8));
  print_string (sprintf "3! %d\n" (factorielle 3));
  print_string (sprintf "%f\n" (3.**5.));
  let p1 = {x=0.0;y=0.0} in
  let p2 = {x=0.0;y=10.0} in
  let p3 = {x=10.0;y=10.0} in
  let p4 = {x=10.0;y=0.0} in
  let polygone = p4::p3::p2::p1::[] in
  print_string (point_to_string (bezier 1. polygone))
  
