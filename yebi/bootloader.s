
read_char:
    read    $1
    bge     $1, $0, read_char_end
    br      read_char
read_char_end:
    ret

halt_code:
    .int    0xc00f0000

.global main
main:
    mov     $5, 0x4001      # write address
    mov     $6, 3           # halt counter
    mov     $7, [halt_code]
    mov     $sp, 1
    shl     $sp, $sp, 20
    mov     $bp, $sp
loop:
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
    mov     [$5], $1
    add     $5, $5, 1
    beq     $1, $7, decr_count
    mov     $6, 3
    br      loop
decr_count:
    sub     $6, $6, 1
    beq     $6, $0, end
    br      loop
end:
    mov     [0x4000], $5
    br      0x4001

