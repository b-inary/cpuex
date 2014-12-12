
# ----------------------------------------------------------------------
#       assembly sample
# ----------------------------------------------------------------------

# comment
; comment

label:

.global main
main:

    # primitive mnemonics
    add     r1, r2, r3, 4
    sub     r1, r2, r3, 4
    shl     r1, r2, r3, 4
    shr     r1, r2, r3, 4
    sar     r1, r2, r3, 4
    and     r1, r2, r3, 4
    or      r1, r2, r3, 4
    xor     r1, r2, r3, 4
    cmpne   r1, r2, r3
    cmpeq   r1, r2, r3
    cmplt   r1, r2, r3
    cmple   r1, r2, r3
    fcmpne  r1, r2, r3
    fcmpeq  r1, r2, r3
    fcmplt  r1, r2, r3
    fcmple  r1, r2, r3
    fadd    r1, r2, r3
    fsub    r1, r2, r3
    fmul    r1, r2, r3
    fdiv    r1, r2, r3
    finv    r1, r2
    fsqrt   r1, r2
    ftoi    r1, r2
    itof    r1, r2
    floor   r1, r2
    ldl     r1, 2
    ldh     r1, r2, 3
    st      r1, r2, 3
    ld      r1, r2, 3
    jl      r1, 2
    jr      r1
    bne     r1, r2, 3
    beq     r1, r2, 3
    fadd.neg r1, r2, r3
    fsub.abs r1, r2, r3
    fmul.abs.neg r1, r2, r3
    bne-    r1, r2, 3
    beq+    r1, r2, 3

    # macros
    nop
    mov     rsp, rbp
    mov     r1, 2
    mov     r1, 234567
    mov     r1, 2.3
    mov     r1, label
    mov     r1, [r2 + 4]
    mov     r1, [label]
    mov     [r1 - 8], r2
    mov     [label], r1
    add     r1, r2, r3
    add     r1, r2, 3
    add     r1, r2, 345678
    and     r1, r2, r3
    and     r1, r2, 3
    and     r1, r2, 345678
    neg     r1, r2
    not     r1, r2
    cmpgt   r1, r2, r3
    cmpge   r1, r2, r3
    fcmpgt  r1, r2, r3
    fcmpge  r1, r2, r3
    read    r1
    write   r1
    br      label
    blt+    r1, r2, label
    bfgt-   r1, r2, label
    push    r1
    pop     r1
    call    label
    ret
    enter   123
    leave
    halt

