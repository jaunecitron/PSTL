open Printf
open Point
       
type environment=
  {
    mutable pos : point;
    mutable positionning : int; (* 0 to absolute; 1 to relative *)
    mutable height : float;
    mutable lift_step : float;
    mutable speed_G0 : float;
    mutable speed_G1 : float;
    mutable speed_extruder_rate : float;
    mutable plastic : float;
    mutable total_plastic : float;
  }
    
let env_init =
  {
    pos={x=0.;y=0.};
    positionning=0;
    height=0.;
    lift_step=0.1;
    speed_G0=9000.;
    speed_G1=1200.;
    speed_extruder_rate=10000.;
    plastic=0.;
    total_plastic=0.
  }

let get_distance_plastic_rate env i =
  if i = 0 then env.speed_G0/.env.speed_extruder_rate else env.speed_G1/.env.speed_extruder_rate
    
let env_to_string env=
  sprintf "env = {\n pos=%s ;\n positionning=%d ;\n height = %f ;\n height_step = %f ;\n plastic=%f ;\n total_plastic=%f }\n"
    (point_to_string env.pos) env.positionning env.height env.lift_step env.plastic env.total_plastic
