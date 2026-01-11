.globl _main
_main:
    stp x29, x30, [sp, #-16]!
    mov x29, sp
    sub sp, sp, #48
    mov w11, #0
    cmp w11, #0
    mov w10, #0
    str w10, [x29, #-16]
    cset w10, eq
    str w10, [x29, #-16]
    ldr w10, [x29, #-16]
    cmp w10, #0
    b.eq and_false1
    mov w10, #2
    str w10, [x29, #-20]
    ldr w10, [x29, #-20]
    mov w11, #1
    add w10, w10, w11
    str w10, [x29, #-20]
    ldr w10, [x29, #-20]
    cmp w10, #1
    mov w10, #0
    str w10, [x29, #-24]
    cset w10, gt
    str w10, [x29, #-24]
    mov w11, #3
    ldr w10, [x29, #-24]
    cmp w10, w11
    mov w10, #0
    str w10, [x29, #-28]
    cset w10, eq
    str w10, [x29, #-28]
    ldr w10, [x29, #-28]
    cmp w10, #0
    b.eq and_false1
    mov w10, #1
    str w10, [x29, #-32]
    b and_end2
and_false1:
    mov w10, #0
    str w10, [x29, #-32]
and_end2:
    ldr w10, [x29, #-32]
    str w10, [x29, #-36]
    ldr w10, [x29, #-36]
    mov w11, #1
    add w10, w10, w11
    str w10, [x29, #-36]
    ldr w0, [x29, #-36]
    add sp, sp, #48
    ldp x29, x30, [sp], #16
    ret
