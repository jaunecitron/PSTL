open Instruction
open Point
open Environment
open Program
open Droite
open Segment
open Printf
open Polygone

let create_box_forme forme =
  let x_min = ref (-1.) in
  let x_max = ref (-1.) in
  let y_min = ref (-1.) in
  let y_max = ref (-1.) in
  let rec aux f =
    match f with
      [] -> {x_min= !x_min ; x_max= !x_max ; y_min = !y_min ; y_max = !y_max}
     |h::t-> (if (h.p.x < !x_min) || (!x_min = -1.) then x_min := h.p.x);
	     (if (h.p.x > !x_max) || (!x_max = -1.) then x_max := h.p.x);
	     (if (h.p.y < !y_min) || (!y_min = -1.) then y_min := h.p.y);
	     (if (h.p.y > !y_max) || (!y_max = -1.) then y_max := h.p.y);
	     (if (h.q.x < !x_min) || (!x_min = -1.) then x_min := h.q.x);
	     (if (h.q.x > !x_max) || (!x_max = -1.) then x_max := h.q.x);
	     (if (h.q.y < !y_min) || (!y_min = -1.) then y_min := h.q.y);
	     (if (h.q.y > !y_max) || (!y_max = -1.) then y_max := h.q.y);
	     aux t in
  aux forme
