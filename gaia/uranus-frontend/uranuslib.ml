
(* floating point functions *)
let fequal x y = x = y
let fless x y = x < y
let fispos x = x > 0.
let fisneg x = x < 0.
let fiszero x = x = 0.
let fneg x = -.x
let fsqr x = x *. x
let fhalf x = x *. 0.5

(* I/O *)
let print_char c =
  #write c
let print_int i =
  #write (i >> 24);
  #write (i >> 16);
  #write (i >> 8);
  #write i
let print_float f =
  print_int (#castint f)
let read_char () =
  #read ()
let read_int () =
  let x1 = #read () in
  let x2 = #read () in
  let x3 = #read () in
  let x4 = #read () in
  (x1 << 24) + (x2 << 16) + (x3 << 8) + x4
let read_float () =
  #castfloat (read_int ())

(* math functions *)
let float_of_int i = #itof i
let int_of_float f = #ftoi f
let floor f = #floor f
(* TODO: sin, cos, atan *)

;;
