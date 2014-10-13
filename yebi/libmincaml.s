
# easy implementation
.global read_int
read_int:
    read    $1
    ret

.global read_float
read_float:
    call read_int
    ret

.global print_char
print_char:
    write   $1
    ret

# easy implementation
.global print_int
print_int:
    write   $1
    ret

.global create_array
create_array:

.global int_of_float
int_of_float:

.global float_of_int
float_of_int:

.global floor
floor:

    ret

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
