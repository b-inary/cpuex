
(* floating point functions *)
let fequal x y = let _ = x +. 0. in x = y (* help type inference *)
let fless x y = let _ = x +. 0. in x < y
let fispos x = x > 0.
let fisneg x = x < 0.
let fiszero x = x = 0.
let fhalf x = x *. 0.5
let fsqr x = x *. x
let fneg x = -.x

(* I/O *)

(* math functions *)

;;
