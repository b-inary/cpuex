
(* floating point operations *)
let fequal x y = x = y
let fless x y = x < y
let fispos x = x > 0.
let fisneg x = x < 0.
let fiszero x = x = 0.
let fneg x = -.x
let fsqr x = x *. x
let fhalf x = x *. 0.5

(* I/O *)
let print_int i =
  #write (i >> 24);
  #write (i >> 16);
  #write (i >> 8);
  #write i
let read_int () =
  let x1 = #read () in
  let x2 = #read () in
  let x3 = #read () in
  let x4 = #read () in
  (x1 << 24) + (x2 << 16) + (x3 << 8) + x4
let print_char c = #write c
let print_float f = print_int (#castint f)
let read_char () = #read ()
let read_float () = #castfloat (read_int ())

(* rounding and type conversion *)
let float_of_int i = #itof i
let int_of_float f = #ftoi f
let floor f = #floor f

(* trigonometric functions *)
let __kernel_sin x =
  let sq = fsqr x in
  x *. (1.0 +. sq *. (-0.16666668 +. sq *. (0.008332824 +. sq *. -0.00019587841)))
let __kernel_cos x =
  let sq = fsqr x in
  1.0 +. sq *. (-0.5 +. sq *. (0.04166368 +. sq *. -0.0013695068))
let __double x upper =
  let d = 2.0 *. x in
  if upper < d then x else __double d upper
let __reduction_loop x p =
  if p < 4. then x else
  __reduction_loop (if x >= p then x -. p else x) (p *. 0.5)
let __reduction x =
  let pi2 = 6.2831853 in
  __reduction_loop x (__double pi2 x)
let sin x =
  let piq = 0.78539816 in
  let pih = 1.57079633 in
  let pi  = 3.14159265 in
  let x1 = __reduction (fabs x) in
  let x2 = if x1 < pi  then x1 else x1 -. pi in
  let x3 = if x2 < pih then x2 else pi -. x2 in
  let abs = if x3 < piq then __kernel_sin x3 else __kernel_cos (pih -. x3) in
  let sign = (0. <= x) = (x1 < pi) in (* true => positive *)
  if sign then abs else fneg abs
let cos x =
  let piq = 0.78539816 in
  let pih = 1.57079633 in
  let pi  = 3.14159265 in
  let x1 = __reduction (fabs x) in
  let x2 = if x1 < pi  then x1 else x1 -. pi in
  let x3 = if x2 < pih then x2 else pi -. x2 in
  let abs = if x3 < piq then __kernel_cos x3 else __kernel_sin (pih -. x3) in
  let sign = (x1 < pi) = (x2 < pih) in
  if sign then abs else fneg abs
let atan x = x (* TODO *)

;;
