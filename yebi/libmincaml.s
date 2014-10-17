
# read 1byte
.global read_char
read_char:
    read    $1
    ret

# write 1byte
.global print_char
print_char:
    write   $1
    ret

# read 4bytes (big endian)
.global read_int
read_int:
.global read_float
read_float:
    read    $1
    read    $2
    read    $3
    read    $4
    shl     $1, $1, 24
    shl     $2, $2, 16
    shl     $3, $3,  8
    add     $1, $1, $2
    add     $3, $3, $4
    add     $1, $1, $3
    ret

# write 4bytes (big endian)
.global print_int
print_int:
.global print_float
print_float:
    shr     $4, $1, 24
    shr     $3, $1, 16
    shr     $2, $1,  8
    write   $4
    write   $3
    write   $2
    write   $1
    ret


# $1: size / $2: initial value
.global create_array
create_array:
    ble     $1, $0, create_array_ret
    mov     $3, [0x4000]
    add     $4, $3, $1
    mov     [0x4000], $4
    sub     $4, $4, 1
create_array_loop:
    mov     [$3], $2
    add     $3, $3, 1
    ble     $3, $4, create_array_loop
    sub     $1, $3, $1
create_array_ret:
    ret


# int -> float
itof_C1:
    .int    0x4b000000      # float(2^23)
itof_C2:
    .int    0x56800000      # float(2^46)
.global float_of_int
float_of_int:
    bge     $1, $0, itof_abs
    neg     $1, $1
    call    itof_abs
    fneg    $1, $1
    ret
itof_abs:
    mov     $3, [itof_C1]
    mov     $4, [itof_C2]
    shr     $2, $1, 23
    shl     $1, $1, 9
    shr     $1, $1, 9
    add     $1, $1, $3
    add     $2, $2, $4
    fneg    $3, $3
    fneg    $4, $4
    fadd    $1, $1, $3
    fadd    $2, $2, $4
    fadd    $1, $1, $2
    ret


# float -> int
ftoi_C1:
    .int    0x00800000      # 2^23
.global int_of_float
int_of_float:
    bge     $1, $0, ftoi_abs
    fneg    $1, $1
    call    ftoi_abs
    neg     $1, $1
    ret
ftoi_abs:
    mov     $3, [ftoi_C1]
    shr     $2, $1, 23
    shl     $1, $1, 9
    shr     $1, $1, 9
    add     $1, $1, $3
    shift   $1, $1, $2 - 149
    add     $1, $1, 1
    shr     $1, $1, 1       # round towards infinity
    ret


# floor function
floor_C1:
    .int    0x4b000000      # float(2^23)
floor_C2:
    .float  1
floor_C3:
    .float  -1
.global floor
floor:
    mov     $2, [floor_C1]
    bge     $1, $2, floor_ret
    fadd    $3, $1, $2
    ble     $3, $0, floor_ret
    bge     $1, $0, floor_L2
    fneg    $3, $1
    fadd    $1, $2, $3
    fneg    $2, $2
    fadd    $1, $1, $2
    bge     $1, $3, floor_L1
    mov     $2, [floor_C2]
    fadd    $1, $1, $2
floor_L1:
    fneg    $1, $1
    ret
floor_L2:
    fadd    $3, $1, $2
    fneg    $2, $2
    fadd    $2, $2, $3
    ble     $2, $1, floor_L3
    mov     $3, [floor_C3]
    fadd    $1, $2, $3
    ret
floor_L3:
    mov     $1, $2
floor_ret:
    ret

# another implementation of floor
# floor2_C1:
    # .float  -1
# .global floor2
# floor2:
    # shl     $2, $1, 1
    # shr     $2, $2, 24
    # mov     $3, 150
    # bge     $2, $3, floor2_ret
    # mov     $3, 126
    # ble     $2, $3, floor2_L1
    # shift   $3, $1, $2 - 150
    # neg     $2, $2
    # shift   $3, $3, $2 + 150
    # br      floor2_L2
# floor2_L1:
    # mov     $3, 0
# floor2_L2:
    # fneg    $2, $1
    # fadd    $2, $2, $3
    # ble     $2, $0, floor2_L3
    # mov     $2, [floor2_C1]
    # fadd    $1, $3, $2
    # ret
# floor2_L3:
    # mov     $1, $3
# floor2_ret:
    # ret


# division
.global finv
finv:
    # TODO
    ret


# square root
.global sqrt
sqrt:
    # TODO
    ret


# sine / cosine function
pi_q:
    .float  0.78539816
pi_h:
    .float  1.57079633
pi_1:
    .float  3.14159265
pi_2:
    .float  6.28318531

.global sin
sin:
    shr     $2, $1, 31
    shl     $1, $1, 1
    shr     $1, $1, 1
    call    reduction
    mov     $3, [pi_1]
    ble     $1, $3, sin_L1
    add     $2, $2, 1
    fneg    $4, $3
    fadd    $1, $1, $4
sin_L1:
    mov     $4, [pi_h]
    ble     $1, $4, sin_L2
    fneg    $1, $1
    fadd    $1, $1, $3
sin_L2:
    mov     $3, [pi_q]
    ble     $1, $3, sin_L3
    fneg    $1, $1
    fadd    $1, $1, $4
    call    kernel_cos
    br      sin_L4
sin_L3:
    call    kernel_sin
sin_L4:
    beq     $1, $0, sin_L5
    shl     $2, $2, 31
    add     $1, $1, $2
sin_L5:
    ret

.global cos
cos:
    mov     $2, 0
    shl     $1, $1, 1
    shr     $1, $1, 1
    call    reduction
    mov     $3, [pi_1]
    ble     $1, $3, cos_L1
    add     $2, $2, 1
    fneg    $4, $3
    fadd    $1, $1, $4
cos_L1:
    mov     $4, [pi_h]
    ble     $1, $4, cos_L2
    add     $2, $2, 1
    fneg    $1, $1
    fadd    $1, $1, $3
cos_L2:
    mov     $3, [pi_q]
    ble     $1, $3, cos_L3
    fneg    $1, $1
    fadd    $1, $1, $4
    call    kernel_sin
    br      cos_L4
cos_L3:
    call    kernel_cos
cos_L4:
    beq     $1, $0, cos_L5
    shl     $2, $2, 31
    add     $1, $1, $2
cos_L5:
    ret

red_C1:
    .int    0x00800000
reduction:
    mov     $5, [pi_1]
    mov     $6, [pi_2]
    mov     $7, [red_C1]
red_L1:
    add     $5, $5, $7
    bge     $1, $5, red_L1
red_L2:
    ble     $1, $5, red_L3
    fneg    $8, $5
    fadd    $1, $1, $8
red_L3:
    sub     $5, $5, $7
    bge     $5, $6, red_L2
    ret

ksin_C0:
    .float   1.0
ksin_C1:
    .float  -0.16666668
ksin_C2:
    .float   0.008332824
ksin_C3:
    .float  -0.00019587841
kernel_sin:
    fmul    $5, $1, $1
    mov     $6, [ksin_C3]
    mov     $7, [ksin_C2]
    mov     $8, [ksin_C1]
    mov     $9, [ksin_C0]
    fmul    $6, $5, $6
    fadd    $6, $6, $7
    fmul    $6, $5, $6
    fadd    $6, $6, $8
    fmul    $6, $5, $6
    fadd    $6, $6, $9
    fmul    $1, $1, $6
    ret

kcos_C0:
    .float   1.0
kcos_C1:
    .float  -0.5
kcos_C2:
    .float   0.04166368
kcos_C3:
    .float  -0.0013695068
kernel_cos:
    fmul    $1, $1, $1
    mov     $5, [kcos_C3]
    mov     $6, [kcos_C2]
    mov     $7, [kcos_C1]
    mov     $8, [kcos_C0]
    fmul    $5, $1, $5
    fadd    $5, $5, $6
    fmul    $5, $1, $5
    fadd    $5, $5, $7
    fmul    $5, $1, $5
    fadd    $1, $5, $8
    ret


# arctangent function
.global atan
atan:
    # TODO
    ret

