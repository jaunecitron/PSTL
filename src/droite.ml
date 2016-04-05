open Point
open Printf

exception NotFound
exception InfiniteResult
       
(* vecteur directeur v (a,b)
   (d) : b*x - a*y + c = 0 *)
type droite = {mutable a:float;mutable b:float;mutable c:float}

let create_droite p1 p2 =
  let coeff_a = p2.x-.p1.x in
  let coeff_b = p2.y-.p1.y in
  let const_c = (coeff_a*.p1.y) -. (coeff_b*.p1.x) in
  {a=coeff_a;b=coeff_b;c=const_c}
    

let droite_to_string droite=
  sprintf "%f*x + %fy + %f" droite.b (-1.*.droite.a) droite.c
	  
let paralleles d1 d2 =
  if ((d1.a*.d2.b) -. (d1.b*.d2.a)) = 0.  then
    begin
      if (d1.a = 0.) then
	d2.b/.d1.b
      else
	d2.a/.d1.a
    end
  else
    0.

let intersection d1 d2 =
  let coeff = paralleles d1 d2 in
  if not (coeff = 0.) then
    begin
      if ((d1.c *. coeff) == d2.c) then
	raise InfiniteResult
      else
	raise NotFound
    end
  else
    if  not (d1.b = 0.) then
      begin
	if not (d2.b = 0.) then
	  let y_res = ( (d2.b *. d1.c) -. (d1.b *. d2.c) ) /. ( (d2.b *. d1.a) -. (d1.b *. d2.a) ) in
	  {x= ((d2.a *. y_res) -. d2.c) /. d2.b;
	   y= y_res}
	else
	  let y_res = d2.c /. d2.a in
	  {x=((d1.a *. y_res) -. d1.c) /. d1.b;
	   y= y_res}
      end
    else
      let y_res = d1.c /. d1.a in
      {x=((d2.a *. y_res) -. d2.c) /. d2.b;
       y= y_res}
