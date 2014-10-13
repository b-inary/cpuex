
.data
    .int    0xdeadbeef
    .float  1.2e34

.text
label:
.global main
main:

    # primitive mnemonics
    add     $1, $2, $3, 4
    sub     $1, $2, $3
    shift   $1, $2, 3
    fneg    $1, $2
    fadd    $1, $2, $3
    fmul    $1, $2, $3
    load    $1, $2, 3
    store   $1, $2, 3
    read    $1
    write   $1
    beq     $1, $2, $3, 4
    ble     $1, $2, $3, 4

    # macros
    nop
    mov     $1, $2
    mov     $1, 2
    mov     $1, $2 + 3
    mov     $1, label
    mov     $1, [$2 + 3]
    mov     $1, [label]
    mov     [$1 + 2], $3
    mov     [label], $1
    add     $1, $2, $3
    add     $1, $2, 3
    sub     $1, $2, 3
    neg     $1, $2
    shl     $1, $2, 3
    shr     $1, $2, 3
    fsub    $1, $2, $3
    br      $1 + 2
    br      label
    beq     $1, $2, $3 + 4
    beq     $1, $2, label
    ble     $1, $2, $3 + 4
    ble     $1, $2, label
    bge     $1, $2, $3 + 4
    bge     $1, $2, label
    push    $1
    pop     $1
    call    $1 + 2
    call    label
    ret
    halt
