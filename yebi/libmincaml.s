
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

# read 4byte (big endian)
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

# write 4byte (big endian)
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
    mov     $3, [0x4000]
    add     $4, $3, $1
    mov     [0x4000], $4
    ble     $1, $0, create_array_ret
    sub     $4, $4, 1
create_array_loop:
    mov     [$3], $2
    add     $3, $3, 1
    ble     $3, $4, create_array_loop
create_array_ret:
    sub     $1, $3, $1
    ret


const_1:
    .int    0x4b000000
const_2:
    .int    0x56800000

# float -> int
.global int_of_float
int_of_float:
    ble     $0, $1, ftoi_L1
    fneg    $1, $1
    call    ftoi_abs
    neg     $1, $1
    ret
ftoi_L1:
    call    ftoi_abs
    ret
ftoi_abs:
    # TODO
    ret

# int -> float
.global float_of_int
float_of_int:
    ble     $0, $1, itof_L1
    neg     $1, $1
    call    itof_abs
    fneg    $1, $1
    ret
itof_L1:
    call    itof_abs
    ret
itof_abs:
    mov     $3, [const_1]
    mov     $4, [const_2]
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

# TODO
.global floor
floor:
    ret


# implement in mincaml?
# .global fdiv
# fdiv:
# .global sqrt
# sqrt:
# .global cos
# cos:
# .global sin
# sin:
# .global atan
# atan:

