
# <library> read 4bytes (big endian)
.global read_int
read_int:
.global read_float
read_float:
    read    r1
    read    r2
    read    r3
    read    r4
    shl     r2, r2,  8
    shl     r3, r3, 16
    shl     r4, r4, 24
    add     r1, r1, r2
    add     r3, r3, r4
    add     r1, r1, r3
    ret

# write 1byte
.global print_char
print_char:
    write   r1
    ret


# <library> create_array(size, initial_value)
.global create_array
create_array:
    ble     r1, 0, array_L2
    shl     r1, r1, 2
    mov     r3, [heap_ptr]
    add     r4, r3, r1
    mov     [heap_ptr], r4
array_L1:
    sub     r4, r4, 4
    mov     [r4], r2
    bne     r3, r4, array_L1
    mov     r1, r3
array_L2:
    ret


# <library> inverse(x)
# no check for exponent = 0, 253-255
# .global finv
# finv:
#     shr     r2, r1, 23
#     mov     r3, 253
#     sub     r2, r3, r2
#     shl     r2, r2, 23
#     mov     r3, 0.5
#     shl     r1, r1, 9
#     shr     r1, r1, 9
#     or      r1, r1, r3
#     mov     r3, -1.8823529
#     mov     r4, 2.8235294
#     mov     r5, 2.0
#     fmul    r3, r1, r3
#     fadd    r3, r3, r4
#     fmul    r4, r1, r3
#     fsub    r4, r5, r4
#     fmul    r3, r3, r4
#     fmul    r4, r1, r3
#     fsub    r4, r5, r4
#     fmul    r3, r3, r4
#     fmul    r4, r1, r3
#     fsub    r4, r5, r4
#     fmul    r3, r3, r4
#     fmul    r1, r2, r3
#     ret


# <library> square_root(x)
# no check for negative number
# .global sqrt
# sqrt:
#     mov     r4, 0.5
#     mov     r5, 1.5
#     mov     r3, 0x5f375a86
#     shr     r2, r1, 1
#     sub     r2, r3, r2
#     fmul    r4, r1, r4
#     fmul    r3, r2, r4
#     fmul    r3, r2, r3
#     fsub    r3, r5, r3
#     fmul    r2, r2, r3
#     fmul    r3, r2, r4
#     fmul    r3, r2, r3
#     fsub    r3, r5, r3
#     fmul    r2, r2, r3
#     fmul    r3, r2, r4
#     fmul    r3, r2, r3
#     fsub    r3, r5, r3
#     fmul    r2, r2, r3
#     fmul    r1, r1, r2
#     ret


.set pi_q, 0x3f490fdb
.set pi_h, 0x3fc90fdb
.set pi_1, 0x40490fdb
.set pi_2, 0x40c90fdb

# <library> sin(x)  [$5-9: reserved]
.global sin
sin:
    enter
    shr     r2, r1, 31
    shl     r1, r1, 1
    shr     r1, r1, 1
    call    reduction
    mov     r3, pi_1
    mov     r4, pi_h
    blt     r1, r3, sin_L1
    xor     r2, r2, 1
    fsub    r1, r1, r3
sin_L1:
    blt     r1, r4, sin_L2
    fsub    r1, r3, r1
sin_L2:
    mov     r3, pi_q
    blt     r1, r3, sin_L3
    fsub    r1, r4, r1
    call    kernel_cos
    br      sin_L4
sin_L3:
    call    kernel_sin
sin_L4:
    shl     r2, r2, 31
    add     r1, r1, r2
    leave
    ret

# <library> cos(x)  [$5-9: reserved]
.global cos
cos:
    enter
    mov     r2, 0
    shl     r1, r1, 1
    shr     r1, r1, 1
    call    reduction
    mov     r3, pi_1
    mov     r4, pi_h
    blt     r1, r3, cos_L1
    xor     r2, r2, 1
    fsub    r1, r1, r3
cos_L1:
    blt     r1, r4, cos_L2
    xor     r2, r2, 1
    fsub    r1, r3, r1
cos_L2:
    mov     r3, pi_q
    blt     r1, r3, cos_L3
    fsub    r1, r4, r1
    call    kernel_sin
    br      cos_L4
cos_L3:
    call    kernel_cos
cos_L4:
    shl     r2, r2, 31
    add     r1, r1, r2
    leave
    ret

# reduction(x)  [$1, $5-9: available]
reduction:
    mov     r5, pi_1
    mov     r6, pi_2
    mov     r7, 0x00800000
red_L1:
    add     r5, r5, r7
    blt     r5, r1, red_L1
red_L2:
    blt     r1, r5, red_L3
    fsub    r1, r1, r5
red_L3:
    sub     r5, r5, r7
    bge     r5, r6, red_L2
    ret

# kernel_sin(x)  [$1, $5-9: available]
kernel_sin:
    fmul    r5, r1, r1
    mov     r6, -0.00019587841
    mov     r7, 0.008332824
    mov     r8, -0.16666668
    mov     r9, 1.0
    fmul    r6, r5, r6
    fadd    r6, r6, r7
    fmul    r6, r5, r6
    fadd    r6, r6, r8
    fmul    r6, r5, r6
    fadd    r6, r6, r9
    fmul    r1, r1, r6
    ret

# kernel_cos(x)  [$1, $5-9: available]
kernel_cos:
    fmul    r1, r1, r1
    mov     r5, -0.0013695068
    mov     r6, 0.04166368
    mov     r7, -0.5
    mov     r8, 1.0
    fmul    r5, r1, r5
    fadd    r5, r5, r6
    fmul    r5, r1, r5
    fadd    r5, r5, r7
    fmul    r5, r1, r5
    fadd    r1, r5, r8
    ret


# <library> arctangent(x)
.global atan
atan:
    enter
    shr     r9, r1, 31
    shl     r9, r9, 31
    shl     r1, r1, 1
    shr     r1, r1, 1
    mov     r2, 0.4375
    bge     r1, r2, atan_L1
    call    kernel_atan
    or      r1, r1, r9
    leave
    ret
atan_L1:
    mov     r2, 2.4375
    bge     r1, r2, atan_L2
    mov     r2, 1.0
    fsub    r8, r1, r2
    fadd    r1, r1, r2
    finv    r1, r1
    fmul    r1, r1, r8
    call    kernel_atan
    mov     r2, pi_q
    fadd    r1, r1, r2
    or      r1, r1, r9
    leave
    ret
atan_L2:
    finv    r1, r1
    call    kernel_atan
    mov     r2, pi_h
    fsub    r1, r2, r1
    or      r1, r1, r9
    leave
    ret

# kernel_atan(x)  [$8, $9: reserved]
kernel_atan:
    mov     r3, 0.060035486
    mov     r4, -0.089764461
    mov     r5, 0.111111104
    mov     r6, -0.142857149
    fmul    r2, r1, r1
    fmul    r3, r2, r3
    fadd    r3, r3, r4
    fmul    r3, r2, r3
    fadd    r3, r3, r5
    fmul    r3, r2, r3
    fadd    r3, r3, r6
    fmul    r3, r2, r3
    mov     r4, 0.2
    mov     r5, -0.3333333
    mov     r6, 1.0
    fadd    r3, r3, r4
    fmul    r3, r2, r3
    fadd    r3, r3, r5
    fmul    r3, r2, r3
    fadd    r3, r3, r6
    fmul    r1, r1, r3
    ret

