
(* floating point operations *)
(* fabs and sqrt are implemented in compiler *)
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
  let sq = x *. x in
  x *. (1.0 +. sq *. (-0.16666669 +. sq *. (0.008332824 +. sq *. -0.00019587841)))
let __kernel_cos x =
  let sq = x *. x in
  1.0 +. sq *. (-0.5 +. sq *. (0.04166368 +. sq *. -0.0013695068))
let __kernel_atan x =
  let sq = x *. x in
  x *. (1.0 +. sq *. (-0.3333333 +. sq *. (0.2 +. sq *. (-0.14285715 +.
    sq *. (0.111111104 +. sq *. (-0.08976446 +. sq *. 0.060035486))))))
let __double x upper =
  let d = 2.0 *. x in
  if upper < d then x else __double d upper
let __reduction_loop x p =
  if p < 4.0 then x else
  __reduction_loop (if x < p then x else x -. p) (p *. 0.5)
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
  let res = if x3 < piq then __kernel_sin x3 else __kernel_cos (pih -. x3) in
  if (0.0 <= x) = (x1 < pi) then res else fneg res
let cos x =
  let piq = 0.78539816 in
  let pih = 1.57079633 in
  let pi  = 3.14159265 in
  let x1 = __reduction (fabs x) in
  let x2 = if x1 < pi  then x1 else x1 -. pi in
  let x3 = if x2 < pih then x2 else pi -. x2 in
  let res = if x3 < piq then __kernel_cos x3 else __kernel_sin (pih -. x3) in
  if (x1 < pi) = (x2 < pih) then res else fneg res
let atan x =
  let piq = 0.78539816 in
  let pih = 1.57079633 in
  let xabs = fabs x in
  if xabs < 0.4375 then __kernel_atan x
  else
    let res =
      if xabs > 2.4375 then pih -. __kernel_atan (1.0 /. xabs)
      else piq +. __kernel_atan ((xabs -. 1.0) /. (xabs +. 1.0)) in
    if x >= 0.0 then res else fneg res

;;
