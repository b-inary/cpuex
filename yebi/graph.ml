
let rec f x = floor x in
let rec g x = 100.0 in

let x_start = -10. in
let x_end   =  10. in
let y_start = -10. in
let y_end   =  10. in

let size = 512 in

(* write header *)
let _ =
    print_char 80;
    print_char (48 + 6);
    print_char 10;
    print_char (48 + 5);
    print_char (48 + 1);
    print_char (48 + 2);
    print_char 32;
    print_char (48 + 5);
    print_char (48 + 1);
    print_char (48 + 2);
    print_char 10;
    print_char (48 + 2);
    print_char (48 + 5);
    print_char (48 + 5);
    print_char 10
in


let x_unit = (x_end -. x_start) /. (float_of_int size) in
let y_unit = (y_end -. y_start) /. (float_of_int size) in

let rec posx x = x_start +. (float_of_int x +. 0.5) *. x_unit in
let rec posy y = y_start +. (float_of_int y +. 0.5) *. y_unit in

let rec print a b c =
    print_char a;
    print_char b;
    print_char c
in

let rec grid x y =
    if fless (fabs x) (0.5 *. x_unit) then true else
    if fless (fabs y) (0.5 *. y_unit) then true else
    if fless (fabs ((float_of_int (int_of_float x)) -. x)) (0.5 *. x_unit) then
        if fless (fabs y) (y_unit *. 5.0) then true else false else
    if fless (fabs ((float_of_int (int_of_float y)) -. y)) (0.5 *. y_unit) then
        if fless (fabs x) (x_unit *. 5.0) then true else false else false
in

let rec loop x y =
    let xx = posx x in
    let yy = posy (size - y - 1) in

    if fless (fabs (f xx -. yy)) (0.5 *. y_unit) then
        print 255 0 0
    else if fless (fabs (g xx -. yy)) (0.5 *. y_unit) then
        print 0 0 255
    else if grid xx yy then
        print 0 0 0
    else
        print 255 255 255;

    if x + 1 < size then
        loop (x + 1) y
    else if y + 1 < size then
        loop 0 (y + 1)
    else
        ()
in

loop 0 0

