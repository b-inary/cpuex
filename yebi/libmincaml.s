
# read 1byte
.global read_char
read_char:
    read    $1
    bge     $1, $0, read_char_end
    br      read_char
read_char_end:
    ret

# <library> write 1byte
.global print_char
print_char:
    write   $1
    ret

# <library> read 4bytes (big endian)
.global read_int
read_int:
.global read_float
read_float:
    call    read_char
    mov     $4, $1
    call    read_char
    mov     $3, $1
    call    read_char
    mov     $2, $1
    call    read_char
    shl     $2, $2,  8
    shl     $3, $3, 16
    shl     $4, $4, 24
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


# <library> create_array(size, initial_value)
.global create_array
create_array:
    ble     $1, $0, array_L2    # if ($1 <= 0) return;
    mov     $3, [0x4000]        # $3 = [0x4000];
    add     $4, $3, $1
    mov     [0x4000], $4        # [0x4000] = $3 + $1;
    sub     $4, $4, 1           # $4 = $3 + $1 - 1;
array_L1:                       # do {
    mov     [$3], $2            #   [$3] = $2;
    add     $3, $3, 1           #   $3 += 1;
    ble     $3, $4, array_L1    # } while ($3 <= $4);
    sub     $1, $3, $1          # return $3 - $1;
array_L2:
    ret


# <library> float_of_int(int)
itof_C1:
    .int    0x4b000000          # float(2**23)
itof_C2:
    .int    0x56800000          # float(2**46)
.global float_of_int
float_of_int:
    bge     $1, $0, itof_abs    # if ($1 < 0) {
    neg     $1, $1
    call    itof_abs            #   $1 = itof_abs(-$1);
    fneg    $1, $1              #   return -$1;
    ret
itof_abs:                       # } else {
    mov     $3, [itof_C1]       #   $3 = float(2**23);
    mov     $4, [itof_C2]       #   $4 = float(2**46);
    shr     $2, $1, 23          #   $2 = $1 >> 23;
    shl     $1, $1, 9
    shr     $1, $1, 9           #   $1 = $1 & ((1 << 23) - 1);
    add     $1, $1, $3          #   $1 += *(int*)&($3);
    add     $2, $2, $4          #   $2 += *(int*)&($4);
    fneg    $3, $3              #   // now $1, $2 have float type
    fneg    $4, $4
    fadd    $1, $1, $3          #   $1 -= $3;
    fadd    $2, $2, $4          #   $2 -= $4;
    fadd    $1, $1, $2          #   return $1 + $2;
    ret                         # }


# <library> int_of_float(float)
ftoi_C1:
    .int    0x00800000          # 2**23
.global int_of_float
int_of_float:
    bge     $1, $0, ftoi_abs    # if ($1 < 0.0) {
    fneg    $1, $1
    call    ftoi_abs            #   $1 = ftoi_abs(-$1);
    neg     $1, $1              #   return -$1;
    ret
ftoi_abs:                       # } else {
    mov     $3, [ftoi_C1]
    shr     $2, $1, 23          #   $2 = exponent($1);
    shl     $1, $1, 9
    shr     $1, $1, 9
    add     $1, $1, $3          #   $1 = fraction($1);
    shift   $1, $1, $2 - 149    #   $1 >>= (127 - exponent + 22);
    add     $1, $1, 1           #   // round towards infinity
    shr     $1, $1, 1           #   return ($1 + 1) >> 1;
    ret                         # }


# <library> floor(x)
# floor_C1:
    # .int    0x4b000000          # float(2**23)
# floor_C2:
    # .float  1.0
# floor_C3:
    # .float  -1.0
# .global floor
# floor:
    # mov     $2, [floor_C1]
    # bge     $1, $2, floor_L4    # if ($1 >= 2**23) return $1;
    # fadd    $3, $1, $2
    # ble     $3, $0, floor_L4    # if ($1 <= -2**23) return $1;
    # bge     $1, $0, floor_L2    # if ($1 < 0) {
    # fneg    $3, $1              #   $3 = fabs($1);
    # fadd    $1, $2, $3          #   $1 = $3 + 2**23;
    # fneg    $2, $2
    # fadd    $1, $1, $2          #   $1 -= 2**23;
    # bge     $1, $3, floor_L1    #   if ($1 < $3) {
    # mov     $2, [floor_C2]
    # fadd    $1, $1, $2          #     $1 += 1.0;
# floor_L1:                       #   }
    # fneg    $1, $1              #   return -$1;
    # ret
# floor_L2:                       # } else {
    # fadd    $3, $1, $2          #   $3 = $1 + 2**23
    # fneg    $2, $2
    # fadd    $2, $2, $3          #   $2 = $3 - 2**23
    # ble     $2, $1, floor_L3    #   if ($2 > $1) {
    # mov     $3, [floor_C3]
    # fadd    $1, $2, $3          #     return $2 - 1.0;
    # ret
# floor_L3:                       #   }
    # mov     $1, $2              #   return $2;
# floor_L4:
    # ret                         # }

# another implementation of floor
floor2_C1:
    .float  -1
.global floor
floor:
    shl     $2, $1, 1
    shr     $2, $2, 24
    mov     $3, 150
    bge     $2, $3, floor2_ret
    mov     $3, 126
    ble     $2, $3, floor2_L1
    shift   $3, $1, $2 - 150
    neg     $2, $2
    shift   $3, $3, $2 + 150
    br      floor2_L2
floor2_L1:
    mov     $3, 0
floor2_L2:
    fneg    $2, $1
    fadd    $2, $2, $3
    ble     $2, $0, floor2_L3
    mov     $2, [floor2_C1]
    fadd    $1, $3, $2
    ret
floor2_L3:
    mov     $1, $3
floor2_ret:
    ret


# <library> inverse(x)
# no check for exponent = 0, 253-255
finv_C1:
    .float  0.5
finv_C2:
    .float  2.0
finv_C3:
    .float  -1.8823529          # -32.0 / 17.0
finv_C4:
    .float   2.8235294          #  48.0 / 17.0
.global finv
finv:
    shr     $2, $1, 23
    neg     $2, $2              # sign($2) = sign($1);
    add     $2, $2, 253         # exponent($2) = 253 - exponent($1);
    shl     $2, $2, 23          # fraction($2) = 0;
    mov     $3, [finv_C1]
    shl     $1, $1, 9
    shr     $1, $1, 9           # sign($1) = 0;
    add     $1, $1, $3          # exponent($1) = 126;
    mov     $3, [finv_C3]
    mov     $4, [finv_C4]
    fmul    $3, $1, $3          # // initial guess
    fadd    $3, $3, $4          # $3 = -32/17 * $1 + 48/17;
    mov     $5, [finv_C2]
    fmul    $4, $1, $3
    fneg    $4, $4
    fadd    $4, $4, $5          # // repeat 3 times
    fmul    $3, $3, $4          # $3 *= 2.0 - $1 * $3;
    fmul    $4, $1, $3
    fneg    $4, $4
    fadd    $4, $4, $5
    fmul    $3, $3, $4
    fmul    $4, $1, $3
    fneg    $4, $4
    fadd    $4, $4, $5
    fmul    $3, $3, $4
    fmul    $1, $2, $3          # return $2 * $3;
    ret


# <library> square_root(x)
# no check for negative number
sqrt_C1:
    .float  0.5
sqrt_C2:
    .float  1.5
sqrt_C3:
    .int    0x5f375a86          # magic number
.global sqrt
sqrt:
    mov     $4, [sqrt_C1]
    mov     $5, [sqrt_C2]
    mov     $3, [sqrt_C3]
    shr     $2, $1, 1           # // initial guess for invsqrt($1)
    sub     $2, $3, $2          # $2 = 0x5f375a86 - ($1 >> 1);
    fmul    $4, $1, $4          # $4 = 0.5 * $1;
    fmul    $3, $2, $4
    fmul    $3, $2, $3
    fneg    $3, $3
    fadd    $3, $3, $5          # // repeat 3 times
    fmul    $2, $2, $3          # $2 *= 1.5 - $2 * $2 * $4;
    fmul    $3, $2, $4
    fmul    $3, $2, $3
    fneg    $3, $3
    fadd    $3, $3, $5
    fmul    $2, $2, $3
    fmul    $3, $2, $4
    fmul    $3, $2, $3
    fneg    $3, $3
    fadd    $3, $3, $5
    fmul    $2, $2, $3
    fmul    $1, $1, $2          # return $1 * $2;
    ret


pi_q:
    .float  0.78539816
pi_h:
    .float  1.57079633
pi_1:
    .float  3.14159265
pi_2:
    .float  6.28318531

# <library> sin(x)  [$5-9: reserved]
.global sin
sin:
    shr     $2, $1, 31          # $2 = sign($1);
    shl     $1, $1, 1
    shr     $1, $1, 1
    call    reduction           # $1 = reduction(fabs($1));
    mov     $3, [pi_1]
    ble     $1, $3, sin_L1      # if ($1 > PI) {
    add     $2, $2, 1           #   $2 = reverse($2);
    fneg    $4, $3
    fadd    $1, $1, $4          #   $1 -= PI;
sin_L1:                         # }
    mov     $4, [pi_h]
    ble     $1, $4, sin_L2      # if ($1 > PI/2) {
    fneg    $1, $1
    fadd    $1, $1, $3          #   $1 = PI - $1;
sin_L2:                         # }
    mov     $3, [pi_q]
    ble     $1, $3, sin_L3      # if ($1 > PI/4) {
    fneg    $1, $1
    fadd    $1, $1, $4
    call    kernel_cos          #   $1 = kernel_cos(PI/2 - $1);
    br      sin_L4
sin_L3:                         # } else {
    call    kernel_sin          #   $1 = kernel_sin($1);
sin_L4:                         # }
    beq     $1, $0, sin_L5      # if ($1 == 0.0) return $1;
    shl     $2, $2, 31
    add     $1, $1, $2          # sign($1) = $2;
sin_L5:
    ret                         # return $1;

# <library> cos(x)  [$5-9: reserved]
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

# reduction(x)  [$1, $5-9: available]
red_C1:
    .int    0x00800000
reduction:
    mov     $5, [pi_1]          # $5 = PI;
    mov     $6, [pi_2]
    mov     $7, [red_C1]
red_L1:                         # do {
    add     $5, $5, $7          #   $5 *= 2.0;
    bge     $1, $5, red_L1      # } while ($1 >= $5);
red_L2:                         # do {
    ble     $1, $5, red_L3      #   if ($1 > $5) {
    fneg    $8, $5
    fadd    $1, $1, $8          #     $1 -= $5;
red_L3:                         #   }
    sub     $5, $5, $7          #   $5 /= 2.0;
    bge     $5, $6, red_L2      # } while ($5 >= 2*PI);
    ret                         # return $1;

# kernel_sin(x)  [$1, $5-9: available]
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

# kernel_cos(x)  [$1, $5-9: available]
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


# <library> arctangent(x)
atan_C1:
    .float  0.4375
atan_C2:
    .float  2.4375
atan_C3:
    .float  -1.0
.global atan
atan:
    shr     $9, $1, 31
    shl     $9, $9, 31          # $9 = sign($1);
    shl     $1, $1, 1
    shr     $1, $1, 1           # $1 = fabs($1);
    mov     $2, [atan_C1]
    bge     $1, $2, atan_L1     # if ($1 < 0.4375) {
    call    kernel_atan         #   $1 = kernel_atan($1);
    add     $1, $1, $9          #   return $1 + $9;
    ret
atan_L1:
    mov     $2, [atan_C2]
    bge     $1, $2, atan_L2     # } else if ($1 < 2.4375) {
    mov     $2, [atan_C3]
    fadd    $8, $1, $2          #   $8 = $1 - 1.0;
    fneg    $2, $2
    fadd    $1, $1, $2
    call    finv                #   $1 = finv($1 + 1.0);
    fmul    $1, $1, $8
    call    kernel_atan         #   $1 = kernel_atan($1 * $8);
    mov     $2, [pi_q]
    fadd    $1, $1, $2          #   $1 += PI/4;
    add     $1, $1, $9          #   return $1 + $9;
    ret                         #
atan_L2:                        # } else {
    call    finv                #   $1 = finv($1);
    call    kernel_atan         #   $1 = kernel_atan($1);
    mov     $2, [pi_h]
    fneg    $1, $1
    fadd    $1, $1, $2          #   $1 = PI/2 - $1;
    add     $1, $1, $9          #   return $1 + $9;
    ret                         # }

# kernel_atan(x)  [$8, $9: reserved]
katan_C0:
    .float   1.0
katan_C1:
    .float  -0.3333333
katan_C2:
    .float   0.2
katan_C3:
    .float  -0.142857149
katan_C4:
    .float   0.111111104
katan_C5:
    .float  -0.089764461
katan_C6:
    .float   0.060035486
kernel_atan:
    mov     $3, [katan_C6]
    mov     $4, [katan_C5]
    mov     $5, [katan_C4]
    mov     $6, [katan_C3]
    fmul    $2, $1, $1
    fmul    $3, $2, $3
    fadd    $3, $3, $4
    fmul    $3, $2, $3
    fadd    $3, $3, $5
    fmul    $3, $2, $3
    fadd    $3, $3, $6
    fmul    $3, $2, $3
    mov     $4, [katan_C2]
    mov     $5, [katan_C1]
    mov     $6, [katan_C0]
    fadd    $3, $3, $4
    fmul    $3, $2, $3
    fadd    $3, $3, $5
    fmul    $3, $2, $3
    fadd    $3, $3, $6
    fmul    $1, $1, $3
    ret

