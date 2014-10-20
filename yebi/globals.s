
.global n_objects
n_objects:
    .int    0

.global objects
objects:
    .int    0, 60

.global screen
screen:
    .float  0.0, 3

.global viewpoint
viewpoint:
    .float  0.0, 3

.global light
light:
    .float  0.0, 3

.global beam
beam:
    .float  255.0

.global and_net
and_net:
    .int    0, 50

.global or_net
or_net:
    .int    0

.global solver_dist
solver_dist:
    .float  0.0

.global intsec_rectside
intsec_rectside:
    .int    0

.global tmin
tmin:
    .float  1000000000.0

.global intersection_point
intersection_point:
    .float  0.0, 3

.global intersected_object_id
intersected_object_id:
    .int    0

.global nvector
nvector:
    .float  0.0, 3

.global texture_color
texture_color:
    .float  0.0, 3

.global diffuse_ray
diffuse_ray:
    .float  0.0, 3

.global rgb
rgb:
    .float  0.0, 3

.global image_size
image_size:
    .int    0, 2

.global image_center
image_center:
    .int    0, 2

.global scan_pitch
scan_pitch:
    .float  0.0

.global startp
startp:
    .float  0.0, 3

.global startp_fast
startp_fast:
    .float  0.0, 3

.global screenx_dir
screenx_dir:
    .float  0.0, 3

.global screeny_dir
screeny_dir:
    .float  0.0, 3

.global screenz_dir
screenz_dir:
    .float  0.0, 3

.global ptrace_dirvec
ptrace_dirvec:
    .float  0.0, 3

.global dirvecs
dirvecs:
    .int    0, 5

.global light_dirvec
light_dirvec:
    .int    0, 2

.global reflections
reflections:
    .int    0, 180

.global n_reflections
n_reflections:
    .int    0


.global init_globals
init_globals:
    mov     $8, 59
    mov     $9, objects
L1:
    mov     $1, 11
    mov     $2, 0
    call    create_array
    add     $2, $8, $9
    mov     [$2], $1
    sub     $8, $8, 1
    bge     $8, $0, L1
    mov     $8, 49
    mov     $9, and_net
L2:
    mov     $1, 1
    mov     $2, -1
    call    create_array
    add     $2, $8, $9
    mov     [$2], $1
    sub     $8, $8, 1
    bge     $8, $0, L2
    mov     $1, 1
    mov     $2, -1
    call    create_array
    mov     $2, $1
    mov     $1, 1
    call    create_array
    mov     [or_net], $1
    mov     $9, light_dirvec
    mov     $1, 3
    mov     $2, 0
    call    create_array
    mov     [$9], $1
    mov     $1, 60
    call    create_array
    mov     [$9 + 1], $1
    mov     $8, 179
    mov     $9, reflections
L3:
    mov     $1, 2
    call    create_array
    mov     $7, $1
    mov     $1, 3
    mov     $2, 0
    call    create_array
    mov     [$1 + 1], $7
    add     $2, $8, $9
    mov     [$2], $1
    sub     $8, $8, 1
    bge     $8, $0, L3
    ret

